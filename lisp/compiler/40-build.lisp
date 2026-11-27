;;;; 40-build.lisp -- helpers for the self-hosting build (see Makefile / docs).

(in-package :cl)

(defun %basename (path) (car (last (%split-string path #\/))))

(defun ng-build-fasls (outdir)
  ;; Compile every system source into OUTDIR/<name>.nfasl.  The sources are
  ;; read in COMMON-LISP, exactly as the stage-0 loader does.
  (let ((*package* (find-package "COMMON-LISP")))
    (dolist (src *system-sources*)
      (compile-file src :output-file (%string-concat outdir "/" (%basename src) ".nfasl"))))
  t)

(defun ng-load-fasls (dir)
  (dolist (src *system-sources*)
    (load-fasl (%string-concat dir "/" (%basename src) ".nfasl")))
  t)

(defun ng-finalize-image ()
  ;; Called just before the final image is saved.
  (%set-export-all-cl nil)
  ;; COMMON-LISP is deliberately NOT locked here: the system sources, the tests and the runners all live in
  ;; COMMON-LISP.  Opt in with (lock-package :cl) -- see docs/COMPATIBILITY.md, "Package locks".
  (setq *source-root* nil)   ; not baked in: the image must not depend on where it was built (see NG_SOURCE_ROOT)
  (setq *record-lambda-expressions* t)
  (setq *package* (find-package "COMMON-LISP-USER"))
  t)
