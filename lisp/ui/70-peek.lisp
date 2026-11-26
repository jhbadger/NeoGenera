;;;; 70-peek.lisp -- Peek: a continuously updated view of processes, memory and windows.
;;;;
;;;; Like Genera's Peek, the window has modes chosen by a key -- P processes, M memory, W windows -- and
;;;; redraws itself every second.  Everything shown is a presentation: click a process to kill, arrest,
;;;; resume it or look at its output or error; click a window to select it.

(in-package :cl)

(define-presentation-type 'process '(t))
(define-presentation-type 'peek-window-entry '(t))
(define-presentation-type 'peek-mode '(t))

(defparameter *peek-modes* '((:processes "P" "Processes") (:memory "M" "Memory") (:windows "W" "Windows")))
(defparameter *peek-interval-ticks* 4)       ; ticks are 250 ms: redraw every second

(defclass peek-window (report-window)
  ((mode :initform :processes :initarg :mode :accessor peek-mode)
   (ticks :initform 0 :accessor peek-ticks)))

;;; ---- content -----------------------------------------------------------------------------------------

(defun %peek-header (w out)
  (dolist (m *peek-modes*)
    (with-output-as-presentation (out (car m) 'peek-mode)
      (with-style (out (if (eq (car m) (peek-mode w)) :bold :dim))
        (format out "~A ~A" (cadr m) (caddr m))))
    (write-string "   " out))
  (with-style (out :dim) (format out "G refresh  Q quit~%~%")))

(defun %fmt-seconds (s)
  (cond ((< s 10) (format nil "~,2Fs" s)) ((< s 600) (format nil "~,1Fs" s)) (t (format nil "~Dm" (floor s 60)))))

(defun %process-line (out p)
  (let* ((info (%process-info p)) (alive (and info t)))
    (with-output-as-presentation (out p 'process)
      (format out "  ~4@A  ~24A" (if info (car info) "") (%abbrev-string (process-name p) 24)))
    (with-style (out (cond ((eq (process-state p) :error) :bold) (alive :normal) (t :dim)))
      (format out " ~22A" (%abbrev-string (if (and (not alive) (process-error p))
                                              (format nil "Error: ~A" (condition-report-string (process-error p)))
                                              (process-state-summary p))
                                          22)))
    (if info
        (format out " ~9@A ~8D" (%fmt-seconds (process-run-time p)) (caddr info))
        (format out " ~9@A ~8A" "" ""))
    (let ((last (%last-output-line p)))
      (when last (with-style (out :dim) (format out "  ~A" (%abbrev-string last 30)))))
    (terpri out)))

(defun %abbrev-string (s n) (if (> (length s) n) (%string-concat (subseq s 0 (max 0 (- n 1))) "~") s))

(defun %last-output-line (p)
  (let ((text (process-output-text p)))
    (when (and text (> (length text) 0))
      (let* ((trimmed (string-right-trim '(#\Newline) text)) (nl (position #\Newline trimmed :from-end t)))
        (subseq trimmed (if nl (+ nl 1) 0))))))

(defun peek-processes (w out)
  (declare (ignore w))
  (with-style (out :bold) (format out "    Id  ~24A ~22A ~9@A ~8@A  ~A~%" "Name" "State" "Time" "Switches" "Output"))
  (dolist (p (all-processes)) (%process-line out p))
  (let ((dead (remove-if #'process-active-p *recent-processes*)))
    (when dead
      (terpri out)
      (with-style (out :bold) (format out "  Recently finished~%"))
      (dolist (p dead) (%process-line out p))))
  (terpri out)
  (with-style (out :dim)
    (format out "  Click a process for commands.  (process-run-function name fn) starts one.~%")))

(defun %kbytes (n) (format nil "~:DK" (floor n 1024)))

(defun peek-memory (w out)
  (declare (ignore w))
  (destructuring-bind (obj cons semi gcs) (%heap-info)
    (with-style (out :bold) (format out "  Heap~%"))
    (format out "    Objects      ~12@A  of ~A~%" (%kbytes obj) (%kbytes semi))
    (format out "    Conses       ~12@A  of ~A~%" (%kbytes cons) (%kbytes semi))
    (format out "    In use       ~12@A~%" (%kbytes (+ obj cons)))
    (format out "    Collections  ~12@A~%" gcs))
  (terpri out)
  (with-style (out :bold) (format out "  System~%"))
  (format out "    Packages     ~12@A~%" (length (list-all-packages)))
  (format out "    Generic fns  ~12@A~%" (length *generic-functions*))
  (format out "    Documents    ~12@A~%" (length *documents*))
  (format out "    Processes    ~12@A~%" (%process-count)))

(defun peek-windows (w out)
  (with-style (out :bold) (format out "  ~30A ~22A ~A~%" "Title" "Kind" "Position"))
  (dolist (win (wm-windows (window-wm w)))
    (with-output-as-presentation (out win 'peek-window-entry)
      (format out "  ~30A" (%abbrev-string (window-title win) 30)))
    (format out " ~22A ~Dx~D at ~D,~D~A~%" (string-downcase (symbol-name (%cls-name (class-of win))))
            (window-w win) (window-h win) (window-x win) (window-y win)
            (if (eq win (wm-focus (window-wm w))) "  (selected)" ""))))

(defun peek-producer (w out)
  (%peek-header w out)
  (case (peek-mode w)
    (:memory (peek-memory w out))
    (:windows (peek-windows w out))
    (t (peek-processes w out))))

;;; ---- behaviour ------------------------------------------------------------------------------------------

(defun peek-refresh (w)
  (let ((back (lw-scroll-back w)))
    (report-refresh w)
    (setf (lw-scroll-back w) back)
    (when (window-wm w) (setf (wm-dirty (window-wm w)) t))))

(defun peek-set-mode (w mode)
  (setf (peek-mode w) mode (window-title w) (format nil "Peek: ~A" (caddr (assoc mode *peek-modes*))))
  (report-refresh w))

(defmethod report-refreshed ((w peek-window)) (setf (lw-scroll-back w) 1000000))

(defmethod window-tick ((w peek-window))
  (setf (peek-ticks w) (+ 1 (peek-ticks w)))
  (when (zerop (mod (peek-ticks w) *peek-interval-ticks*)) (peek-refresh w)))

(defmethod window-key ((w peek-window) key)
  (let ((c (key-code key)) (m (key-mods key)))
    (let ((mode (and (integerp c) (zerop (logand m (logior +ctrl+ +meta+)))
                     (car (find (string-upcase (string (code-char c))) *peek-modes* :key #'cadr :test #'string=)))))
      (if mode (progn (peek-set-mode w mode) t) (call-next-method)))))

(defun open-peek (wm &optional (mode :processes))
  "Open (or select) the Peek window in MODE (:processes, :memory or :windows)."
  (let ((existing (find-if (lambda (x) (typep x 'peek-window)) (wm-windows wm))))
    (if existing
        (progn (wm-raise wm existing) (peek-set-mode existing mode) existing)
        (let ((win (open-report wm "Peek: Processes" #'peek-producer :class 'peek-window :w 96 :h 18)))
          (unless (eq mode :processes) (peek-set-mode win mode))
          win))))

(defun %peek-refresh-all ()
  (when *wm* (dolist (w (wm-windows *wm*)) (when (typep w 'peek-window) (peek-refresh w)))))

;;; ---- presentation commands --------------------------------------------------------------------------------

(define-presentation-command (peek-mode peek-mode :documentation "Show this" :default t) (mode)
  (when *wm* (open-peek *wm* mode)))

(define-presentation-command (select-peek-window peek-window-entry :documentation "Select" :default t) (win)
  (when (and *wm* (member win (wm-windows *wm*))) (wm-raise *wm* win)))

(defun %show-process-text (title text)
  (if *wm*
      (open-report *wm* title (lambda (w out) (declare (ignore w)) (write-string (if (and text (> (length text) 0)) text "(nothing)") out) (terpri out)))
      (format t "~&~A~%~A~%" title (or text ""))))

(define-presentation-command (describe-process process :documentation "Describe" :default t) (p)
  (format t "~&~A~%  state ~A, whostate ~A, ~A of processor time~@[~%  error: ~A~]~%"
          p (string-downcase (symbol-name (process-state p))) (process-whostate p) (%fmt-seconds (process-run-time p))
          (and (process-error p) (condition-report-string (process-error p)))))
(define-presentation-command (show-process-output process :documentation "Show output") (p)
  (%show-process-text (format nil "Output of ~A" (process-name p)) (process-output-text p)))
(define-presentation-command (show-process-error process :documentation "Show error and backtrace") (p)
  (if (process-error p)
      (%show-process-text (format nil "Error in ~A" (process-name p))
                          (with-output-to-string (s)
                            (format s "~A~%~%" (condition-report-string (process-error p)))
                            (let ((i 0)) (dolist (f (process-backtrace p)) (%print-frame i f s) (setq i (+ i 1))))))
      (format t "~&~A has no error~%" (process-name p))))
(define-presentation-command (kill-process process :documentation "Kill") (p)
  (process-kill p) (%peek-refresh-all))
(define-presentation-command (arrest-process process :documentation "Arrest") (p)
  (process-arrest p) (%peek-refresh-all))
(define-presentation-command (unarrest-process process :documentation "Un-arrest") (p)
  (process-unarrest p) (%peek-refresh-all))

;;; ---- Listener commands -------------------------------------------------------------------------------------

(define-command (peek :documentation "Open Peek: processes, memory and windows, updated every second") ((mode word :default "processes"))
  (let ((m (find-if (lambda (e) (%word-prefix-p (string-upcase mode) (string-upcase (caddr e)))) *peek-modes*)))
    (if *wm* (open-peek *wm* (if m (car m) :processes)) (show-processes))))

(define-command (show-processes :documentation "List the processes") ()
  (show-processes))

(define-command (kill-process :documentation "Kill a process (by name or id)") ((name word :prompt "Process"))
  (let ((p (find-process name)))
    (cond ((null p) (format t "~&No process ~A~%" name))
          (t (process-kill p) (format t "~&Killed ~A~%" (process-name p))))))

;;; ---- errors in background processes are announced in the mouse documentation line --------------------------------

(defun %announce-process-error (p c)
  (when *wm*
    (setf (wm-message *wm*) (format nil "Process ~A got an error: ~A  (:peek to look)" (process-name p) (condition-report-string c))
          (wm-dirty *wm*) t)))
(setq *process-error-hook* #'%announce-process-error)

(define-select-activity #\P "Peek" (lambda (w) (typep w 'peek-window)) #'open-peek)
