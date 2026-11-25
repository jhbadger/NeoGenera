CC      ?= cc
CFLAGS  ?= -O2 -g -std=gnu11 -Wall -Wextra -Wno-unused-parameter -fno-strict-aliasing
LDFLAGS ?= -lm -lpthread
SRC      = src/memory.c src/symbols.c src/stream.c src/reader.c src/printer.c \
           src/eval.c src/vm.c src/prims_core.c src/prims_data.c src/prims_num.c src/bignum.c src/term.c src/prims_sys.c src/prims_io.c src/main.c
BIN      = build/neogenera
NG       = $(BIN) --no-toplevel
LISP_SRC = $(wildcard lisp/*/*.lisp lisp/sources.lisp)

.PHONY: all kernel stage0 stage1 stage2 selfhost world test test-c test-lisp test-pty test-stress asan clean

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
