;;;; 50-manual.lisp -- the manuals, imported from docs/manual/*.md when the system is built.

(in-package :cl)

(import-markdown :user-guide "docs/manual/user-guide.md" :category :guide
                 :keywords (start listener commands tutorial introduction))
(import-markdown :language-reference "docs/manual/language-reference.md" :category :reference
                 :keywords (lisp numbers clos conditions streams compiler language))
(import-markdown :window-system "docs/manual/window-system.md" :category :manual
                 :keywords (window mouse presentation terminal themes listener))
(import-markdown :editor-manual "docs/manual/editor-manual.md" :category :manual
                 :keywords (editor emacs undo search dired git lisp-mode))
(import-markdown :tools-manual "docs/manual/tools-manual.md" :category :manual
                 :keywords (inspector debugger profiler trace step cross-reference tools))
(import-markdown :processes "docs/manual/processes.md" :category :manual
                 :keywords (process processes peek lock thread scheduling multiprocessing))
(import-markdown :documentation-system "docs/manual/documentation-system.md" :category :manual
                 :keywords (documentation docstring markdown export html pdf search tutorial))

(import-markdown :flavors-manual "docs/manual/flavors-manual.md" :category :manual
                 :keywords (flavors flavor defflavor send message-passing vanilla daemon whopper wrapper symbolics genera oop))
(import-markdown :math-manual "docs/manual/math-manual.md" :category :manual
                 :keywords (math statistics matrix linear-algebra random distribution regression plot data-frame symbolic))
(import-markdown :bio-manual "docs/manual/bio-manual.md" :category :manual
                 :keywords (bioinformatics sequence dna protein fasta alignment blast phylogeny genome))
(import-markdown :pascal-manual "docs/manual/pascal-manual.md" :category :manual
                 :keywords (pascal compiler units dialect iso vs templates electric pascal-mode tool-kit))
(import-markdown :fortran-manual "docs/manual/fortran-manual.md" :category :manual
                 :keywords (fortran f77 ftn compiler format fixed-format electric fortran-mode tool-kit symbolics))

(import-markdown :bytecode-manual "docs/manual/bytecode-manual.md" :category :manual
                 :keywords (bytecode assembler assembly defasm vm instructions verifier asm-mode disassemble stack-machine))

;;; Coverage is measured over what exists now; symbols defined later (user code, tests) are not counted.
(snapshot-core-symbols)
