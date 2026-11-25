;;;; sources.lisp -- the ordered list of system source files.
;;;; Shared by the stage-0 boot, the self-hosting build and the tests.

(setq *package* (find-package "COMMON-LISP"))

(defparameter *system-sources*
  '("lisp/core/00-base.lisp"
    "lisp/core/10-places.lisp"
    "lisp/core/20-seq.lisp"
    "lisp/core/30-struct.lisp"
    "lisp/core/40-format.lisp"
    "lisp/core/50-loop.lisp"
    "lisp/core/52-clos.lisp"
    "lisp/core/54-conditions.lisp"
    "lisp/core/56-streams.lisp"
    "lisp/core/58-pprint.lisp"
    "lisp/core/59-debugger.lisp"
    "lisp/core/60-listener.lisp"
    "lisp/core/90-test.lisp"
    "lisp/compiler/00-defs.lisp"
    "lisp/compiler/10-analyze.lisp"
    "lisp/compiler/20-codegen.lisp"
    "lisp/compiler/30-driver.lisp"
    "lisp/compiler/40-build.lisp"
    "lisp/ui/00-screen.lisp"
    "lisp/ui/10-wm.lisp"
    "lisp/ui/20-present.lisp"
    "lisp/ui/30-commands.lisp"
    "lisp/ui/40-listener.lisp"
    "lisp/ui/50-ui.lisp"))
