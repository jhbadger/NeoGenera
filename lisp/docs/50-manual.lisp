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
(import-markdown :documentation-system "docs/manual/documentation-system.md" :category :manual
                 :keywords (documentation docstring markdown export html pdf search tutorial))

(import-markdown :math-manual "docs/manual/math-manual.md" :category :manual
                 :keywords (math statistics matrix linear-algebra random distribution regression plot data-frame symbolic))
;;; Coverage is measured over what exists now; symbols defined later (user code, tests) are not counted.
(snapshot-core-symbols)
