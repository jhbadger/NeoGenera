# NeoGenera 1.0

NeoGenera 1.0 is out: a Lisp environment in the spirit of the Symbolics Genera workstations, implemented in C and in itself.

It is a small C kernel and a Common Lisp subset written in Lisp, with a compiler that compiles itself to a byte-identical fixed point; a text-mode window system in
which every printed object is clickable; an Emacs-style editor, Inspector, profiler and the rest of a development environment; hypertext documentation you can search,
bookmark, run and export; and libraries that make it useful for work: linear algebra and statistics validated against R and NIST, and bioinformatics from FASTA to
phylogenies with sequences, alignments and trees you can click on.

**Try it:** `make world && bin/neogenera --ui`, then `:tutorial`. A recorded tour is in `docs/tour.cast`; ten example programs are in `examples/`.

**What we are proud of:** every parser is fuzzed under sanitizers; running out of memory or stack is an error you can handle, not a crash; images are checksummed and written
atomically; builds are reproducible; the compatibility and security documents say plainly what NeoGenera does and does not do.

**What it is not:** a drop-in SBCL. Strings are UTF-8 bytes, arrays are one-dimensional, the numeric tower has no complex type, CLOS is a practical subset, and the garbage collector is
not generational. `docs/COMPATIBILITY.md` lists every deliberate deviation.

Thank you to everyone who read the roadmap and kept it honest.
