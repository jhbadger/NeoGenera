#!/bin/sh
# Build a Debian package from the staged install tree.  Usage: build-deb.sh VERSION
set -e
VERSION="${1:?version}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ARCH="$(dpkg --print-architecture 2>/dev/null || echo amd64)"
umask 022
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT      # staged outside the source tree: dpkg-deb insists on sane directory modes
STAGE="$WORK/neogenera_${VERSION}_${ARCH}"
rm -rf "$ROOT/build/deb" && mkdir -p "$STAGE/DEBIAN" "$ROOT/build/deb"
make -C "$ROOT" install DESTDIR="$STAGE" PREFIX=/usr >/dev/null
chmod -R u+rwX,go+rX-w "$STAGE"; chmod 755 "$STAGE/DEBIAN"
SIZE=$(du -sk "$STAGE/usr" | cut -f1)
cat > "$STAGE/DEBIAN/control" <<CTRL
Package: neogenera
Version: $VERSION
Section: devel
Priority: optional
Architecture: $ARCH
Installed-Size: $SIZE
Depends: libc6, gzip
Recommends: git
Suggests: ncbi-blast+, samtools, minimap2
Maintainer: NeoGenera maintainers <neogenera@example.invalid>
Homepage: https://example.invalid/neogenera
Description: Lisp environment evoking the Symbolics Genera workstations
 NeoGenera is a Common Lisp subset with a self-hosting bytecode compiler, a
 text-mode window system, an Emacs-style editor, developer tools, an integrated
 hypertext documentation system and libraries for mathematics, statistics and
 bioinformatics.
CTRL
gzip -9n "$STAGE/usr/share/man/man1/neogenera.1"
(cd "$STAGE" && find usr -type f -print0 | sort -z | xargs -0 md5sum > DEBIAN/md5sums)
dpkg-deb --root-owner-group --build "$STAGE" "$ROOT/build/deb/neogenera_${VERSION}_${ARCH}.deb" >/dev/null
echo "wrote build/deb/neogenera_${VERSION}_${ARCH}.deb"
