CC      ?= cc
CFLAGS  ?= -O2 -g -std=gnu11 -Wall -Wextra -Wno-unused-parameter -fno-strict-aliasing \
           -ffile-prefix-map=$(CURDIR)=. -fdebug-prefix-map=$(CURDIR)=.
LDFLAGS ?= -lm -lpthread
SRC      = src/memory.c src/symbols.c src/stream.c src/reader.c src/printer.c \
           src/eval.c src/vm.c src/prims_core.c src/prims_data.c src/prims_num.c src/bignum.c src/term.c src/prims_sys.c src/prims_io.c src/prims_bio.c src/main.c
BIN      = build/neogenera
NG       = $(BIN) --no-toplevel
LISP_SRC = $(wildcard lisp/*/*.lisp lisp/sources.lisp docs/manual/*.md)

.PHONY: all kernel stage0 stage1 stage2 selfhost world test test-c test-lisp test-pty test-stress asan clean \
        bench soak fuzz conformance install dist deb repro-check examples docs site release-check

all: world

kernel: $(BIN)
$(BIN): $(SRC) src/neogenera.h
	@mkdir -p build
	$(CC) $(CFLAGS) -o $@ $(SRC) $(LDFLAGS)

# stage 0: the C evaluator loads the whole system (compiler included)
stage0: build/stage0.img
build/stage0.img: $(BIN) $(LISP_SRC) lisp/boot.lisp
	$(NG) --no-image --load lisp/boot.lisp --save-image $@

# stage 1: the interpreted compiler compiles the system; load the result
stage1: build/stage1.img
build/stage1.img: build/stage0.img
	@mkdir -p build/s1
	$(NG) -i build/stage0.img --eval '(ng-build-fasls "build/s1")'
	$(NG) -i build/stage0.img --eval '(ng-load-fasls "build/s1")' --eval '(ng-use-compiler-eval)' --save-image $@

# stage 2: the compiled compiler recompiles the system
stage2: build/s2.done
build/s2.done: build/stage1.img
	@mkdir -p build/s2
	$(NG) -i build/stage1.img --eval '(ng-build-fasls "build/s2")'
	@touch $@

# fixed point: stage-1 and stage-2 output must be byte-identical
selfhost: build/s2.done
	@for f in build/s1/*.nfasl; do \
	  cmp -s $$f build/s2/$$(basename $$f) || { echo "FIXED POINT FAILED: $$f"; exit 1; }; \
	done; echo "fixed point reached: stage1 == stage2 ($$(ls build/s1/*.nfasl | wc -l) files, $$(cat build/s1/*.nfasl | wc -c) bytes)"

world: build/neogenera.img
build/neogenera.img: build/stage1.img selfhost
	$(NG) -i build/stage1.img --eval '(ng-load-fasls "build/s2")' --eval '(ng-use-compiler-eval)' \
	      --eval '(ng-finalize-image)' --save-image $@

test: test-c test-lisp test-pty
test-c: $(BIN)
	@sh tests/c/run.sh
test-lisp: world
	$(BIN) -i build/neogenera.img --no-toplevel --load tests/lisp/run-all.lisp

test-pty: world
	python3 tests/ui/pty_test.py

# every GC safe point collects: finds missing roots
test-stress: world
	NG_STRESS=1 $(BIN) -i build/neogenera.img --gc-stress --no-toplevel --load tests/lisp/run-all.lisp

asan:
	@mkdir -p build
	$(CC) -O1 -g -std=gnu11 -fno-strict-aliasing -fsanitize=address,undefined -fno-omit-frame-pointer \
	      -o build/neogenera-asan $(SRC) $(LDFLAGS)

clean:
	rm -rf build

# ---- performance, soak and fuzz runs ---------------------------------------------------------------------------

bench: world
	$(BIN) -i build/neogenera.img --no-toplevel --load bench/bench.lisp

soak: world
	NG_SOAK_SECONDS=$${NG_SOAK_SECONDS:-60} $(BIN) -i build/neogenera.img --no-toplevel --load tests/soak/soak.lisp

# the fuzz tests are part of `make test`; this runs them under ASan+UBSan as well
fuzz: world asan
	ASAN_OPTIONS=detect_leaks=0 build/neogenera-asan -i build/neogenera.img --no-toplevel --load tests/lisp/run-all.lisp

conformance: world
	NG_CONF_REPORT=docs/CONFORMANCE.md $(BIN) -i build/neogenera.img --no-toplevel --load tests/conformance/run.lisp

# ---- documentation and the example gallery ------------------------------------------------------------------------

docs: world
	rm -rf build/docs && mkdir -p build/docs
	$(BIN) -i build/neogenera.img --no-toplevel --eval '(export-documentation "build/docs")'

examples: world
	@sh examples/run-all.sh

site: docs
	@sh packaging/make-site.sh

# ---- installation and packaging -----------------------------------------------------------------------------------------

VERSION  := $(shell grep -o 'ng-version[*] "[0-9.]*"' lisp/core/60-listener.lisp | grep -o '[0-9][0-9.]*')
PREFIX   ?= /usr/local
DESTDIR  ?=
ARCH     := $(shell uname -m)
OS       := $(shell uname -s | tr A-Z a-z)
DIST      = neogenera-$(VERSION)-$(OS)-$(ARCH)

install: world docs
	install -d $(DESTDIR)$(PREFIX)/bin $(DESTDIR)$(PREFIX)/lib/neogenera $(DESTDIR)$(PREFIX)/share/neogenera \
	        $(DESTDIR)$(PREFIX)/share/doc/neogenera/html $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 755 bin/neogenera $(DESTDIR)$(PREFIX)/bin/neogenera
	install -m 755 build/neogenera $(DESTDIR)$(PREFIX)/lib/neogenera/neogenera
	install -m 644 build/neogenera.img $(DESTDIR)$(PREFIX)/lib/neogenera/neogenera.img
	cp -R lisp docs examples $(DESTDIR)$(PREFIX)/share/neogenera/
	install -m 644 LICENSE README.md $(DESTDIR)$(PREFIX)/share/doc/neogenera/
	cp -R build/docs/. $(DESTDIR)$(PREFIX)/share/doc/neogenera/html/
	install -m 644 packaging/neogenera.1 $(DESTDIR)$(PREFIX)/share/man/man1/neogenera.1

# a prebuilt world: the binary and its image (they must travel together), sources, documentation, checksums
dist: world docs
	rm -rf build/dist && mkdir -p build/dist
	$(MAKE) install DESTDIR=$(CURDIR)/build/dist/$(DIST) PREFIX=/usr
	cd build/dist && tar --sort=name --owner=0 --group=0 --numeric-owner --mtime='2020-01-01 00:00Z' -cf - $(DIST) | gzip -9n > $(DIST).tar.gz
	cd build/dist && (sha256sum $(DIST).tar.gz > SHA256SUMS 2>/dev/null || shasum -a 256 $(DIST).tar.gz > SHA256SUMS)
	@echo "wrote build/dist/$(DIST).tar.gz"; cat build/dist/SHA256SUMS

deb: world docs
	@sh packaging/build-deb.sh $(VERSION)

# two clean builds in different directories must produce byte-identical images (and binaries)
repro-check:
	@sh packaging/repro-check.sh

release-check: test repro-check
	@echo "release checks passed"
