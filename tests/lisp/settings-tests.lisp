;;;; settings-tests.lisp -- the settings registry, the settings file, the commands and the Settings window.
;;;; Everything uses a throw-away settings file under /tmp, and restores the globals it changes.

(in-package :cl)

(defvar *st-dir* (format nil "/tmp/ng-settings-test-~D-~D" (get-universal-time) (get-internal-real-time))
  "A directory name unique to this run: a fixed name collides with leftovers owned by another user.")
(defvar *st-pwned* nil)
(defun st-path () (%string-concat *st-dir* "/settings.lisp"))
(defun st-clean () (when (probe-file (st-path)) (delete-file (st-path))))
(defun st-entry (name) (cadr (assq name (%settings-read-entries (st-path)))))
(defun st-has-entry (name) (and (assq name (%settings-read-entries (st-path))) t))

(defun call-with-test-settings (fn)
  (let ((old-theme (theme-name *theme*)) (old-save *save-history*) (old-tile *tile-mode*)
        (old-bm *doc-bookmarks*) (old-hist *saved-history*) (old-main *main-listener*)
        (old-base *settings-baseline*) (old-path *settings-path*))
    (%make-directory *st-dir*)
    (st-clean)
    (setq *main-listener* nil *settings-path* (st-path))
    (unwind-protect (funcall fn)
      (st-clean)
      (set-theme old-theme)
      (setq *save-history* old-save *tile-mode* old-tile *doc-bookmarks* old-bm *saved-history* old-hist
            *main-listener* old-main *settings-baseline* old-base *settings-path* old-path))))

(deftest settings-values
  (call-with-test-settings
   (lambda ()
     (is (find-setting :theme))
     (is (find-setting :tile-mode))
     (is (find-setting :save-history))
     (is (find-setting :history))
     (is (find-setting :bookmarks))
     (set-setting :theme :green)
     (is-equal :green (theme-name *theme*))
     (is-equal :green (setting-value :theme))
     (signals-error (set-setting :theme :no-such-theme))
     (signals-error (set-setting :save-history :maybe))
     (signals-error (setting-value :no-such-setting))
     (is-equal :green (setting-value :theme))                 ; a refused value changes nothing
     (reset-setting :theme)
     (is-equal :classic (setting-value :theme))
     (set-setting :save-history nil)
     (is-equal nil *save-history*)
     (set-setting :tile-mode :grid)
     (is-equal :grid *tile-mode*)
     (is-equal :grid (setting-value :tile-mode))
     ;; history is trimmed to its limit and non-strings are dropped
     (set-setting :history (list "a" 2 "b"))
     (is-equal '("a" "b") (setting-value :history))
     (set-setting :history (let ((acc nil)) (dotimes (i 150) (setq acc (cons (format nil "c~D" i) acc))) acc))
     (is-equal *history-limit* (length (setting-value :history))))))

(deftest settings-file-roundtrip
  (call-with-test-settings
   (lambda ()
     (is-equal (st-path) (settings-path))
     (set-setting :theme :green)
     (set-setting :tile-mode :grid)
     (set-setting :save-history t)
     (set-setting :history (list "(+ 1 2)" "second line"))
     (set-setting :bookmarks (list :user-guide))
     (is-equal (st-path) (settings-save))
     (is (probe-file (st-path)))
     (is-equal :green (st-entry :theme))
     (is-equal :grid (st-entry :tile-mode))
     (is-equal '("(+ 1 2)" "second line") (st-entry :history))
     (is-equal '(:user-guide) (st-entry :bookmarks))
     ;; saving makes the preferences clean; changing one makes it dirty again
     (is (not (setting-dirty-p :theme)))
     (set-setting :theme :amber)
     (is (setting-dirty-p :theme))
     (is (not (setting-dirty-p :history)))                     ; session data is never "unsaved"
     ;; a new session picks everything up again
     (set-setting :tile-mode :main)
     (set-setting :history nil)
     (set-setting :bookmarks nil)
     (is-equal '(:theme :tile-mode :graphics :cursor :save-history :history :bookmarks) (settings-load))
     (is-equal :green (theme-name *theme*))
     (is-equal :grid *tile-mode*)
     (is-equal '("(+ 1 2)" "second line") (setting-value :history))
     (is-equal '(:user-guide) *doc-bookmarks*)
     (is-equal nil *settings-warnings*))))

(deftest settings-file-is-data-only
  (call-with-test-settings
   (lambda ()
     (setq *st-pwned* nil)
     (set-setting :theme :classic) (set-setting :tile-mode :horizontal) (set-setting :save-history t)
     (write-file-lines (st-path)
                       (list ";; hand edited"
                             "(:theme :amber)"
                             "(:no-such-setting 42)"
                             "(:tile-mode :sideways)"
                             "(:save-history #.(setq *st-pwned* t))"
                             "(:history (\"one\" 2 \"three\"))"
                             "(:tile-mode :grid"
                             "(:bookmarks (:user-guide :no-such-topic-xyz))"))
     (let ((applied (settings-load)))
       (is-equal '(:theme :history :bookmarks) applied))
     (is-equal :amber (theme-name *theme*))
     (is-equal :horizontal *tile-mode*)                        ; invalid and unreadable entries change nothing
     (is-equal t *save-history*)
     (is-equal nil *st-pwned*)                                 ; #. is never evaluated
     (is-equal '("one" "three") (setting-value :history))
     (is-equal '(:user-guide) *doc-bookmarks*)                 ; a bookmark without a page is dropped
     (is (>= (length *settings-warnings*) 2))
     ;; a missing file is not an error
     (st-clean)
     (is-equal nil (settings-load)))))

(deftest settings-session-save
  (call-with-test-settings
   (lambda ()
     (set-setting :theme :classic) (set-setting :save-history t)
     (set-setting :history (list "x"))
     ;; no settings file yet: exiting must not create one
     (settings-save-session)
     (is (not (probe-file (st-path))))
     ;; with a file, exit refreshes the session data but not unsaved preferences
     (set-setting :theme :green)
     (settings-save)
     (set-setting :theme :amber)
     (set-setting :history (list "y" "x"))
     (settings-save-session)
     (is-equal :green (st-entry :theme))
     (is-equal '("y" "x") (st-entry :history))
     ;; history is left out of the file when the user does not want it kept
     (set-setting :save-history nil)
     (settings-save)
     (is (not (st-has-entry :history)))
     (is-equal nil (st-entry :save-history))
     (is (st-has-entry :save-history))
     ;; no home directory: saving says so
     (let ((*settings-path* nil))
       (when (null (settings-path)) (signals-error (settings-save)))))))

(deftest settings-history-follows-main-listener
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignore wm))
    (call-with-test-settings
     (lambda ()
       (setq *main-listener* lw *save-history* t)
       (setf (lw-history lw) (list "b" "a"))
       (is-equal '("b" "a") (setting-value :history))
       (settings-save)
       (is-equal '("b" "a") (st-entry :history))
       (setf (lw-history lw) nil)
       (settings-load)
       (is-equal '("b" "a") (lw-history lw))))))

(deftest settings-commands
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignore lw))
    (call-with-test-settings
     (lambda ()
       (set-setting :theme :classic) (set-setting :save-history t)
       (type-line wm ":set")
       (is (screen-has wm "tile-mode"))
       (is (screen-has wm "save-history"))
       (type-line wm ":set theme amber")
       (is-equal :amber (theme-name *theme*))
       (is (screen-has wm "theme is now amber"))
       (type-line wm ":set theme")
       (is (screen-has wm "theme is amber"))
       (type-line wm ":set theme chartreuse")
       (is (screen-has wm "must be one of"))
       (is-equal :amber (theme-name *theme*))
       (type-line wm ":set save-history off")
       (is-equal nil *save-history*)
       (type-line wm ":set save-history yes")
       (is-equal t *save-history*)
       (type-line wm ":set tile grid")                         ; unique prefix of a name
       (is-equal :grid *tile-mode*)
       (type-line wm ":set history x")
       (is (screen-has wm "cannot be changed"))
       (type-line wm ":set nosuch 1")
       (is (screen-has wm "No such setting"))
       (is (not (probe-file (st-path))))
       (type-line wm ":save-settings")
       (is (probe-file (st-path)))
       (is (screen-has wm "Settings saved to"))
       (is-equal :amber (st-entry :theme))
       (is (%string-search ":set" (documentation-of-command-help) 0))))))

(defun documentation-of-command-help ()
  (with-output-to-string (s) (let ((*standard-output* s)) (execute-command-line "help"))))

(deftest settings-cursor
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignore lw))
    (call-with-test-settings
     (lambda ()
       (set-setting :cursor :blink)
       (type-line wm ":cursor")
       (is (screen-has wm "The cursor is blink"))
       (type-line wm ":cursor solid")
       (is-equal :solid *cursor-style*)
       (is-equal :solid (setting-value :cursor))
       ;; classic paints a black block; the terminal's cursor must not cover it when solid
       (set-theme :classic)
       (wm-draw wm)
       (is (not (scr-cursor-visible (wm-screen wm))))
       (let ((cell (screen-cell (wm-screen wm) (scr-cursor-row (wm-screen wm)) (scr-cursor-col (wm-screen wm)))))
         (is-equal (tc :cursor) (nth 2 cell)))
       (set-setting :cursor :blink)
       (wm-draw wm)
       (is (scr-cursor-visible (wm-screen wm)))
       (set-setting :cursor :solid)
       (type-line wm ":set cursor blink")
       (is-equal :blink *cursor-style*)
       (type-line wm ":cursor flashing")
       (is (screen-has wm "must be one of"))
       (is-equal :blink *cursor-style*)
       (set-setting :cursor :solid)
       (is-equal :solid (progn (settings-save) (st-entry :cursor)))
       (set-setting :cursor :blink)))))

(deftest settings-window
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignore lw))
    (call-with-test-settings
     (lambda ()
       (set-setting :theme :classic) (set-setting :tile-mode :horizontal) (set-setting :save-history t)
       (settings-mark-clean)
       (type-line wm ":settings")
       (let ((w (find-settings-window wm)))
         (is w)
         (is (typep w 'settings-window))
         (is (screen-has wm "NeoGenera Settings"))
         (is (screen-has wm "[Save settings]"))
         (is (screen-has wm "(x) classic"))
         (is (screen-has wm "( ) green"))
         ;; clicking a value applies it and redraws the window
         (click-at wm (screen-has wm "( ) green") 2)
         (is-equal :green (theme-name *theme*))
         (is (screen-has wm "(x) green"))
         (is (screen-has wm "Theme *"))                         ; unsaved change is flagged
         (click-at wm (screen-has wm "( ) grid") 2)
         (is-equal :grid (setting-value :tile-mode))
         (is-equal :grid (wm-layout wm))                        ; applied to the window manager at once
         ;; saving from the window
         (is (not (probe-file (st-path))))
         (click-at wm (screen-has wm "[Save settings]") 2)
         (is (probe-file (st-path)))
         (is-equal :green (st-entry :theme))
         (is-equal :grid (st-entry :tile-mode))
         (is (not (screen-has wm "Theme *")))
         ;; keys: d restores defaults, r reverts to the saved file
         (wm-feed wm "d")
         (is-equal :classic (theme-name *theme*))
         (is-equal :horizontal (setting-value :tile-mode))
         (wm-feed wm "r")
         (is-equal :green (theme-name *theme*))
         (is-equal :grid (setting-value :tile-mode))
         ;; opening it again reuses the window
         (type-line wm ":settings")
         (is-equal 1 (length (remove-if-not (lambda (x) (typep x 'settings-window)) (wm-windows wm))))
         ;; q closes it
         (wm-raise wm (find-settings-window wm))
         (wm-feed wm "q")
         (is (null (find-settings-window wm))))))))
