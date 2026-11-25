;;;; 65-settings.lisp -- user settings: a registry, a data file, commands and the Settings window.
;;;;
;;;; A SETTING has a name (a keyword), a kind (:choice, :boolean or :list), a getter and a setter.
;;;; Changing a setting applies it at once; SETTINGS-SAVE writes the settings to
;;;; ~/.neogenera/settings.lisp (or $NEOGENERA_HOME/settings.lisp), and NG-UI loads that file at
;;;; startup.  The file holds one entry per line, (:name value), and is data only: it is read with
;;;; *READ-EVAL* off and never evaluated, and an entry that is unreadable or invalid is skipped.
;;;;
;;;; Preferences (theme, tile-mode, save-history) are saved only when the user asks.  Session data
;;;; (history, bookmarks) is refreshed in an existing settings file when the window system exits.
;;;; Other files register their own settings with DEFINE-SETTING (the documentation viewer does
;;;; so for bookmarks).  Commands: :settings (the window), :set, :save-settings.

(in-package :cl)

(defstruct (setting (:constructor %make-setting) (:copier nil))
  name kind doc default choices getter setter validate session persist-p)

(defvar *settings* nil "All registered settings, in display order.")
(defvar *settings-path* nil "When non-NIL, the settings file to use instead of the default location.")
(defvar *settings-baseline* nil "Alist (name . value) of the preferences as last saved or loaded.")
(defvar *settings-warnings* nil "Problems met by the last SETTINGS-LOAD, newest first.")
(defvar *main-listener* nil "The Listener whose command history is saved.")
(defvar *saved-history* nil "Command history loaded from the settings file, used until a Listener exists.")
(defvar *history-limit* 100 "How many history entries are kept in the settings file.")
(defvar *save-history* t "True when command history is kept in the settings file.")
(defvar *tile-mode* :horizontal "The window layout applied when the window system starts.")

;;; ---- the registry -----------------------------------------------------------------------------------

(defun define-setting (name kind doc &key default choices get set validate session persist-if)
  "Register the setting NAME (a keyword) of KIND :choice, :boolean or :list.  GET returns its value and
SET applies a new one.  CHOICES (a list or a function returning one) are the legal values of a :choice;
VALIDATE checks and cleans a :list value.  A SESSION setting is kept up to date in the settings file at
exit; PERSIST-IF, when given, is a function that says whether the setting is written at all."
  (let ((s (%make-setting :name name :kind kind :doc doc :default default :choices choices :getter get :setter set
                          :validate validate :session session :persist-p persist-if)))
    (if (find-setting name)
        (setq *settings* (mapcar (lambda (x) (if (eq (setting-name x) name) s x)) *settings*))
        (setq *settings* (append *settings* (list s))))
    s))

(defun find-setting (name)
  "The setting called NAME, or NIL."
  (dolist (s *settings* nil) (when (eq (setting-name s) name) (return s))))

(defun %need-setting (name)
  (or (find-setting name) (error "No such setting: ~A  (:set lists them)" name)))

(defun setting-choice-list (s)
  "The legal values of the :choice setting S."
  (let ((c (setting-choices s))) (if (functionp c) (funcall c) c)))

(defun %setting-label (s)
  (string-capitalize (substitute #\Space #\- (symbol-name (setting-name s)))))

(defun %validate-setting (s value)
  (case (setting-kind s)
    (:choice (unless (member value (setting-choice-list s))
               (error "~A must be one of: ~{~A~^ ~}" (string-downcase (symbol-name (setting-name s)))
                      (mapcar (lambda (c) (string-downcase (symbol-name c))) (setting-choice-list s))))
             value)
    (:boolean (unless (or (eq value t) (null value))
                (error "~A must be on or off" (string-downcase (symbol-name (setting-name s)))))
              value)
    (t (if (setting-validate s) (funcall (setting-validate s) value) value))))

(defun setting-value (name)
  "The current value of the setting NAME."
  (funcall (setting-getter (%need-setting name))))

(defun set-setting (name value)
  "Check VALUE and apply it to the setting NAME now (it is saved only by SETTINGS-SAVE).  Returns the value."
  (let* ((s (%need-setting name)) (v (%validate-setting s value)))
    (funcall (setting-setter s) v)
    v))

(defun reset-setting (name)
  "Put the setting NAME back to its default value."
  (set-setting name (setting-default (%need-setting name))))

(defun %settings-snapshot ()
  (mapcar (lambda (s) (cons (setting-name s) (funcall (setting-getter s)))) *settings*))

(defun settings-mark-clean ()
  "Remember the current preferences as the saved state (what the Settings window compares against)."
  (setq *settings-baseline* (%settings-snapshot))
  nil)

(defun setting-dirty-p (name)
  "True when the preference NAME differs from the saved state.  Session data is never dirty."
  (let* ((s (%need-setting name)) (b (assq name *settings-baseline*)))
    (and b (not (setting-session s)) (not (equal (cdr b) (funcall (setting-getter s)))))))

(defun %session-setting-names ()
  (mapcar #'setting-name (remove-if-not #'setting-session *settings*)))

;;; ---- the file ---------------------------------------------------------------------------------------

(defun settings-path ()
  "The settings file: *SETTINGS-PATH*, else $NEOGENERA_HOME/settings.lisp, else ~/.neogenera/settings.lisp.
NIL when there is no home directory."
  (or *settings-path*
      (let ((nh (getenv "NEOGENERA_HOME")) (home (getenv "HOME")))
        (cond ((and nh (> (length nh) 0)) (%string-concat nh "/settings.lisp"))
              ((and home (> (length home) 0)) (%string-concat home "/.neogenera/settings.lisp"))
              (t nil)))))

(defun %settings-file-lines (path)
  (with-open-file (s path)
    (let ((acc nil) (eof (list nil)))
      (loop (let ((line (read-line s nil eof)))
              (when (eq line eof) (return))
              (setq acc (cons line acc))))
      (nreverse acc))))

(defun %settings-chunks (lines)
  "Group LINES into entries: an entry starts at a line beginning with \"(:\"; comments and junk are dropped."
  (let ((chunks nil) (cur nil) (nl (string #\Newline)))
    (dolist (l lines)
      (if (%starts-with l "(:")
          (progn (when cur (setq chunks (cons (%join-strings (reverse cur) nl) chunks)))
                 (setq cur (list l)))
          (when cur (setq cur (cons l cur)))))
    (when cur (setq chunks (cons (%join-strings (reverse cur) nl) chunks)))
    (nreverse chunks)))

(defun %settings-warn (fmt &rest args)
  (setq *settings-warnings* (cons (apply #'format nil fmt args) *settings-warnings*)))

(defun %settings-read-entries (path)
  "The (name value) entries of the settings file PATH.  Data only: *READ-EVAL* is off, and an entry that
cannot be read is skipped with a warning."
  (let ((entries nil) (*read-eval* nil) (*package* (find-package "COMMON-LISP")))
    (when (and path (probe-file path))
      (dolist (chunk (%settings-chunks (%settings-file-lines path)))
        (handler-case
            (let ((form (%read-from-string chunk 0)))
              (when (and (consp form) (keywordp (car form)) (consp (cdr form)) (null (cddr form)))
                (setq entries (cons form entries))))
          (error (e) (%settings-warn "skipped an unreadable entry: ~A" e)))))
    (nreverse entries)))

(defun settings-load (&optional (path (settings-path)) preferences-only)
  "Apply the settings saved in PATH.  Entries that are unknown, unreadable or invalid are skipped; the
reasons are left in *SETTINGS-WARNINGS*.  With PREFERENCES-ONLY, session data (history, bookmarks) is left
alone.  Returns the names applied."
  (setq *settings-warnings* nil)
  (let ((applied nil))
    (dolist (e (%settings-read-entries path))
      (let ((s (find-setting (car e))))
        (when (and s (not (and preferences-only (setting-session s))))
          (handler-case (progn (set-setting (car e) (cadr e)) (setq applied (cons (car e) applied)))
            (error (err) (%settings-warn "~A: ~A" (string-downcase (symbol-name (car e))) err))))))
    (nreverse applied)))

(defun %setting-persistent-p (s)
  (or (null (setting-persist-p s)) (funcall (setting-persist-p s))))

(defun settings-save (&optional names (path (settings-path)))
  "Write the settings to PATH, creating its directory.  With NAMES, only those settings are updated and the
other entries of an existing file are kept.  Returns PATH."
  (unless path (error "No place to keep settings: HOME is not set (or set NEOGENERA_HOME)"))
  (let* ((targets (if names (mapcar #'%need-setting names) *settings*))
         (target-names (mapcar #'setting-name targets))
         (entries (if names
                      (remove-if (lambda (e) (member (car e) target-names)) (%settings-read-entries path))
                      nil))
         (slash (position #\/ path :from-end t)))
    (dolist (s targets)
      (when (%setting-persistent-p s)
        (setq entries (append entries
                              (list (list (setting-name s) (%validate-setting s (funcall (setting-getter s)))))))))
    (when (and slash (> slash 0)) (%make-directory (subseq path 0 slash)))
    (let ((tmp (%string-concat path ".tmp")))
      (with-open-file (s tmp :direction :output)
        (format s ";;;; NeoGenera settings, written by :save-settings and the Settings window.~%")
        (format s ";;;; Data only: every entry is (:name value) and is never evaluated.~%")
        (let ((*package* (find-package "COMMON-LISP")))
          (dolist (e entries) (prin1 e s) (terpri s))))
      (unless (%rename-file tmp path) (error "Cannot write ~A" path)))
    (unless names (settings-mark-clean))
    path))

(defun settings-save-session ()
  "At exit: when a settings file already exists, refresh its session data (history, bookmarks).  Never signals."
  (let ((path (settings-path)))
    (when (and path (probe-file path))
      (handler-case (settings-save (%session-setting-names) path)
        (error (e) (declare (ignore e)) nil)))))

(defun settings-revert ()
  "Forget unsaved changes: every preference goes back to its saved value (its default if none is saved)."
  (dolist (s *settings*) (unless (setting-session s) (reset-setting (setting-name s))))
  (settings-load (settings-path) t)
  (settings-mark-clean)
  nil)

(defun settings-restore-defaults ()
  "Put every preference back to its default (not saved until SETTINGS-SAVE)."
  (dolist (s *settings*) (unless (setting-session s) (reset-setting (setting-name s))))
  nil)

;;; ---- the built-in settings -------------------------------------------------------------------------

(defun %validate-history (v)
  (unless (listp v) (error "history must be a list of strings"))
  (let ((acc nil) (n 0))
    (dolist (x v)
      (when (and (stringp x) (< n *history-limit*)) (setq acc (cons x acc)) (setq n (+ n 1))))
    (nreverse acc)))

(define-setting :theme :choice "Colour theme of the window system (F8 cycles)."
  :default :classic :choices #'theme-names
  :get (lambda () (theme-name *theme*))
  :set (lambda (v) (set-theme v) (when *wm* (screen-invalidate (wm-screen *wm*)))))

(define-setting :tile-mode :choice "How windows are tiled (F7 and :tile change it too)."
  :default :horizontal :choices '(:horizontal :vertical :main :grid)
  :get (lambda () (if (and *wm* (not (eq (wm-layout *wm*) :overlap))) (wm-layout *wm*) *tile-mode*))
  :set (lambda (v) (setq *tile-mode* v) (when *wm* (wm-tile *wm* v))))

(define-setting :save-history :boolean "Keep your command history between sessions."
  :default t
  :get (lambda () *save-history*)
  :set (lambda (v) (setq *save-history* v)))

(define-setting :history :list "Commands typed in the main Listener, newest first."
  :session t :default nil :persist-if (lambda () *save-history*)
  :validate #'%validate-history
  :get (lambda () (if *main-listener* (lw-history *main-listener*) *saved-history*))
  :set (lambda (v)
         (setq *saved-history* v)
         (when *main-listener* (setf (lw-history *main-listener*) (copy-list v)))))

;;; ---- commands --------------------------------------------------------------------------------------

(defun %format-setting-value (s v)
  (case (setting-kind s)
    (:choice (string-downcase (symbol-name v)))
    (:boolean (if v "on" "off"))
    (t (if (listp v) (format nil "~D item~:P" (length v)) (prin1-to-string v)))))

(defun %find-setting-by-name (text)
  (let ((down (string-downcase text)) (hits nil))
    (dolist (s *settings*)
      (let ((n (string-downcase (symbol-name (setting-name s)))))
        (when (string= n down) (return-from %find-setting-by-name s))
        (when (%starts-with n down) (setq hits (cons s hits)))))
    (cond ((null hits) (error "No such setting: ~A  (:set lists them)" text))
          ((cdr hits) (error "Ambiguous setting: ~A" text))
          (t (car hits)))))

(defun %parse-setting-value (s text)
  (let ((text (string-downcase (string-trim " " text))) (name (string-downcase (symbol-name (setting-name s)))))
    (case (setting-kind s)
      (:choice (or (dolist (c (setting-choice-list s) nil)
                     (when (string= text (string-downcase (symbol-name c))) (return c)))
                   (error "~A must be one of: ~{~A~^ ~}" name
                          (mapcar (lambda (c) (string-downcase (symbol-name c))) (setting-choice-list s)))))
      (:boolean (cond ((member text '("on" "yes" "true" "t" "1") :test #'string=) t)
                      ((member text '("off" "no" "false" "nil" "0") :test #'string=) nil)
                      (t (error "~A must be on or off" name))))
      (t (error "~A cannot be changed with :set (use the Settings window)" name)))))

(defun %print-settings (&optional (out *standard-output*))
  (dolist (s *settings*)
    (format out "  ~A ~A~A~%" (%pad-to (string-downcase (symbol-name (setting-name s))) 13)
            (%pad-to (%format-setting-value s (funcall (setting-getter s))) 12)
            (if (setting-dirty-p (setting-name s)) "* " "  "))
    (format out "      ~A~%" (setting-doc s))))

(define-command (set :documentation "Show or change a setting: :set NAME VALUE (:save-settings keeps it)")
    ((name word :default "") (value string :default ""))
  (cond ((string= name "")
         (format t "~&Settings (* = changed, not saved yet; :settings opens the window):~%")
         (%print-settings))
        (t (let ((s (%find-setting-by-name name)))
             (if (string= value "")
                 (format t "~&~A is ~A~%" (string-downcase (symbol-name (setting-name s)))
                         (%format-setting-value s (funcall (setting-getter s))))
                 (progn (set-setting (setting-name s) (%parse-setting-value s value))
                        (format t "~&~A is now ~A  (:save-settings keeps it)~%" (string-downcase (symbol-name (setting-name s)))
                                (%format-setting-value s (funcall (setting-getter s))))))))))

(define-command (save-settings :documentation "Save the settings to the settings file") ()
  (format t "~&Settings saved to ~A~%" (settings-save)))

(define-command (settings :documentation "Open the Settings window (theme, layout, history, ...)") ()
  (if *wm* (open-settings-window *wm*) (format t "~&The Settings window needs the window system (try :set)~%")))

;;; ---- the Settings window -----------------------------------------------------------------------------

(defclass settings-window (report-window) ())

(define-presentation-type 'setting-choice '(t))
(define-presentation-type 'settings-action '(t))

(defun %refresh-settings-windows ()
  (when *wm*
    (dolist (w (wm-windows *wm*))
      (when (typep w 'settings-window)
        (let ((back (lw-scroll-back w)))          ; redraw in place: keep the scroll position
          (report-refresh w)
          (setf (lw-scroll-back w) back))))))

;; like documents, the Settings window is read from the top (also after it is resized or re-tiled)
(defmethod report-refreshed ((w settings-window)) (setf (lw-scroll-back w) 1000000))
(defmethod window-resized :after ((w settings-window)) (setf (lw-scroll-back w) 1000000))

(defun %settings-note (text)
  (when *wm* (setf (wm-message *wm*) text)))

(defun %settings-action (action)
  "Carry out ACTION (:save :revert :defaults :clear-history :clear-bookmarks) and redraw the Settings window."
  (handler-case
      (case action
        (:save (%settings-note (format nil "Settings saved to ~A" (settings-save))))
        (:revert (settings-revert) (%settings-note "Back to the saved settings"))
        (:defaults (settings-restore-defaults) (%settings-note "Defaults restored (not saved yet)"))
        (:clear-history (set-setting :history nil))
        (:clear-bookmarks (when (find-setting :bookmarks) (set-setting :bookmarks nil))))
    (error (e) (%settings-note (format nil "Error: ~A" e))))
  (%refresh-settings-windows))

(define-presentation-command (choose-setting setting-choice :documentation "Use this value" :default t) (choice)
  (set-setting (car choice) (cdr choice))
  (%refresh-settings-windows))

(define-presentation-command (settings-action settings-action :documentation "Do this" :default t) (action)
  (%settings-action action))

(defun %present-choice (out name value label selected)
  (with-style (out :link)
    (with-output-as-presentation (out (cons name value) 'setting-choice)
      (write-string (%string-concat (if selected "(x) " "( ) ") label) out)))
  (write-string "  " out))

(defun %present-action (out action label)
  (with-style (out :link)
    (with-output-as-presentation (out action 'settings-action) (write-string (%string-concat "[" label "]") out)))
  (write-string "  " out))

(defun %list-count (name) (let ((v (and (find-setting name) (setting-value name)))) (if (listp v) (length v) 0)))

(defun settings-producer ()
  "The producer that draws the Settings window."
  (lambda (w out)
    (declare (ignore w))
    (with-style (out :bold) (format out "NeoGenera Settings~%"))
    (with-style (out :dim)
      (format out "Click a value to use it now.  * marks a change that is not saved yet.~%")
      (format out "Keys: s save, r revert, d defaults, q close~%"))
    (terpri out)
    (%present-action out :save "Save settings")
    (%present-action out :revert "Revert to saved")
    (%present-action out :defaults "Restore defaults")
    (terpri out)
    (dolist (s *settings*)
      (unless (setting-session s)
        (let ((name (setting-name s)) (v (funcall (setting-getter s))))
          (terpri out)
          (with-style (out :bold) (format out "~A~A~%" (%setting-label s) (if (setting-dirty-p name) " *" "")))
          (with-style (out :dim) (format out "  ~A~%" (setting-doc s)))
          (write-string "  " out)
          (case (setting-kind s)
            (:choice (dolist (c (setting-choice-list s))
                       (%present-choice out name c (string-downcase (symbol-name c)) (eq c v))))
            (:boolean (%present-choice out name t "on" (eq v t))
                      (%present-choice out name nil "off" (null v))))
          (terpri out))))
    (terpri out)
    (with-style (out :bold) (format out "Session data~%"))
    (format out "  Command history: ~D~%    " (%list-count :history))
    (%present-action out :clear-history "Clear history")
    (terpri out)
    (when (find-setting :bookmarks)
      (format out "  Documentation bookmarks: ~D~%    " (%list-count :bookmarks))
      (%present-action out :clear-bookmarks "Clear bookmarks")
      (terpri out))
    (with-style (out :dim)
      (format out "  Refreshed in the settings file when you quit, once it exists.~%"))
    (terpri out)
    (with-style (out :dim)
      (let ((path (settings-path)))
        (format out "File: ~A~A~%" (or path "(no home directory)")
                (if (and path (probe-file path)) "" "  (not saved yet)"))))))

(defmethod window-key ((w settings-window) key)
  (let ((c (key-code key)) (m (key-mods key)))
    (cond ((and (eql c 115) (zerop m)) (%settings-action :save) t)
          ((and (eql c 114) (zerop m)) (%settings-action :revert) t)
          ((and (eql c 100) (zerop m)) (%settings-action :defaults) t)
          (t (call-next-method)))))

(defun find-settings-window (wm)
  "The Settings window of WM, or NIL."
  (dolist (w (wm-windows wm) nil) (when (typep w 'settings-window) (return w))))

(defun open-settings-window (wm)
  "Open the Settings window (or raise the one that is open)."
  (let ((w (find-settings-window wm)))
    (if w
        (progn (report-refresh w) (wm-raise wm w) w)
        (open-report wm "Settings" (settings-producer) :class 'settings-window :w 64 :h 24))))
