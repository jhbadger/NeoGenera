;;;; 20-asm-listing.lisp -- turning compiled code back into assembly source.
;;;;
;;;; ASM-DECOMPILE reads the instruction stream of any compiled function (one the compiler produced or one DEFASM
;;;; assembled) and returns a DEFASM form that assembles to the same instructions; ASM-LISTING prints it with
;;;; addresses.  Jump targets become labels (L<address>), parameters get their names, nested code becomes a
;;;; (:lambda ...) operand of closure.

(in-package :cl)

(defvar *asm-comment-column* 36 "The column trailing comments are aligned to in assembly listings and ASM mode.")
(defvar *asm-instruction-indent* 4 "Columns an instruction is indented inside a DEFASM form (ASM mode and listings).")
(defvar *asm-label-indent* 2 "Columns a label is indented inside a DEFASM form (ASM mode and listings).")

(defun %asm-readable-p (x &optional (depth 0))
  (cond ((> depth 8) nil)
        ((or (null x) (numberp x) (characterp x) (stringp x) (symbolp x)) t)
        ((consp x) (and (%asm-readable-p (car x) (+ depth 1)) (%asm-readable-p (cdr x) (+ depth 1))))
        ((simple-vector-p x) (every (lambda (e) (%asm-readable-p e (+ depth 1))) x))
        (t nil)))

(defun %asm-param-names (parts)
  "(values lambda-list names) for the code described by PARTS: the declared lambda list when it is plain, else P0 P1 ..."
  (let* ((nreq (nth 1 parts)) (nopt (nth 2 parts)) (rest (and (nth 3 parts) t)) (arglist (nth 6 parts))
         (n (+ nreq nopt (if rest 1 0)))
         (declared (and (listp arglist)
                        (handler-case
                            (multiple-value-bind (a b c names) (%asm-parse-lambda-list arglist)
                              (and (= a nreq) (= b nopt) (eq c rest) (= (length names) n) names))
                          (asm-error () nil)))))
    (if declared
        (values arglist declared)
        (let ((names nil) (ll nil))
          (dotimes (i n) (setq names (cons (make-symbol (format nil "P~D" i)) names)))
          (setq names (nreverse names))
          (let ((p names))
            (dotimes (i nreq) (setq ll (cons (car p) ll)) (setq p (cdr p)))
            (when (> nopt 0)
              (setq ll (cons '&optional ll))
              (dotimes (i nopt) (setq ll (cons (car p) ll)) (setq p (cdr p))))
            (when rest (setq ll (cons '&rest ll)) (setq ll (cons (car p) ll))))
          (values (nreverse ll) names)))))

(defun %asm-decode (code &optional nfree)
  "(values lambda-list entries parts unsafe) for CODE, a function or code object.  ENTRIES: (:label address symbol) and
(:ins address form).  UNSAFE is true when the routine cannot pass the verifier's type proofs.  NFREE is the number of
captured variables of a nested routine."
  (let* ((parts (%code-parts code)) (instrs (nth 4 parts)) (consts (nth 5 parts))
         (words (coerce instrs 'vector)) (n (length words)) (decoded nil) (targets nil) (pc 0))
    (multiple-value-bind (ll names) (%asm-param-names parts)
      (loop
        (when (>= pc n) (return))
        (let* ((e (find (aref words pc) *opcodes* :key #'cadr)))
          (unless e (error 'asm-error :message (format nil "Bad opcode ~D at address ~D" (aref words pc) pc) :routine (nth 0 parts)))
          (let ((args nil))
            (dotimes (i (caddr e)) (setq args (cons (aref words (+ pc 1 i)) args)))
            (setq args (nreverse args))
            (when (member (car e) '(:jmp :jnil :jnnil)) (pushnew (car args) targets))
            (setq decoded (cons (list pc (car e) args) decoded))
            (setq pc (+ pc 1 (caddr e))))))
      (setq decoded (nreverse decoded))
      (let ((label-syms nil) (entries nil))
        (dolist (a (sort (copy-list targets) #'<))
          (setq label-syms (cons (cons a (make-symbol (format nil "L~D" a))) label-syms)))
        (flet ((slot-operand (i) (if (< i (length names)) (nth i names) i))
               (const-at (k) (aref consts k)))
          (dolist (d decoded)
            (let* ((addr (car d)) (op (cadr d)) (args (caddr d)) (lab (assoc addr label-syms)))
              (when lab (setq entries (cons (list :label addr (cdr lab)) entries)))
              (let ((head (intern (symbol-name op) :cl)) (form nil))
                (setq form
                      (case op
                        (:const (let ((c (const-at (car args))))
                                  (if (%asm-readable-p c) (list head c) (list head nil))))
                        ((:lref :lset) (list head (slot-operand (car args))))
                        ((:gref :gset :gfun :bind) (list head (const-at (car args))))
                        ((:jmp :jnil :jnnil) (list head (cdr (assoc (car args) label-syms))))
                        (:closure (let ((c (const-at (car args))))
                                    (if (%codep c)
                                        (multiple-value-bind (nll nentries nparts nunsafe) (%asm-decode c (cadr args))
                                          (declare (ignore nparts))
                                          (list head (list* :lambda nll (append (and nunsafe (list (list :unsafe))) (mapcar #'caddr nentries)))
                                                (cadr args)))
                                        (list head nil (cadr args)))))
                        (:chktype (list head (car (find (car args) *asm-type-codes* :key #'cdr))))
                        (:chkrange (let ((c (const-at (car args)))) (list head (car c) (cdr c))))
                        (t (cons head args))))
                (setq entries (cons (list :ins addr form) entries))))))
        (let ((end (assoc n label-syms))) (when end (setq entries (cons (list :label n (cdr end)) entries))))
        (setq entries (nreverse entries))
        (let ((used (let ((m 0)) (dolist (d decoded) (when (eq (cadr d) :fref) (setq m (max m (+ 1 (car (caddr d))))))) m)))
          (values ll entries parts
                  (not (handler-case (progn (%asm-code nil ll (mapcar #'caddr entries) (or nfree (and (> used 0) used))) t)
                         (asm-error () nil)))
                  used))))))

(defun asm-decompile (function)
  "Return a DEFASM form that assembles to the same instructions as the compiled FUNCTION (or the name of one).  Code the
compiler produced under (safety 0) is not provable, so its form carries (:unsafe)."
  (let* ((fn (if (symbolp function) (symbol-function function) function)))
    (unless (ignore-errors (%code-parts fn)) (error "ASM-DECOMPILE: ~S is not a compiled function" function))
    (multiple-value-bind (ll entries parts unsafe) (%asm-decode fn)
      (let* ((name (let ((n (nth 0 parts))) (or n (and (symbolp function) function) 'anonymous)))
             (items (mapcar #'caddr entries))
             (doc (and (symbolp name) (get name '%doc-function))))
        (when unsafe (setq items (cons (list :unsafe) items)))
        (when doc (setq items (cons doc items)))
        (list* 'defasm name ll items)))))

;;; ---- printing ---------------------------------------------------------------------------------------------------------------------

(defun %asm-sym-text (s)
  (cond ((null s) "nil")
        ((and (symbolp s) (null (symbol-package s))) (string-downcase (symbol-name s)))
        (t (prin1-to-string s))))

(defun %asm-operand-text (x)
  (cond ((and (consp x) (eq (car x) :lambda)) "(:lambda ...)")
        ((symbolp x) (%asm-sym-text x))
        (t (prin1-to-string x))))

(defun %asm-form-text (form)
  "The text of one instruction: (mnemonic operand ...), mnemonic in lower case."
  (with-output-to-string (s)
    (write-char #\( s)
    (when (keywordp (car form)) (write-char #\: s))
    (write-string (string-downcase (symbol-name (car form))) s)
    (dolist (x (cdr form)) (write-char #\Space s) (write-string (%asm-operand-text x) s))
    (write-char #\) s)))

(defun %asm-pad-to (s col)
  (if (< (length s) col) (concatenate 'string s (make-string (- col (length s)) :initial-element #\Space)) s))

(defun %asm-print-entries (entries stream ind comment-p)
  "Print ENTRIES (as returned by %ASM-DECODE) one per line: labels at IND-2, instructions at IND, addresses as comments."
  (let ((last (car (last entries))))
    (dolist (e entries)
      (let ((lastp (eq e last)))
        (cond
          ((eq (car e) :label)
           (format stream "~&~A~A" (make-string (max 0 (- ind 2)) :initial-element #\Space) (%asm-sym-text (caddr e)))
           (when lastp (write-string ")" stream)))
          (t
           (let* ((form (caddr e)) (nested (and (eq (car form) 'closure) (consp (cadr form)) (eq (car (cadr form)) :lambda))))
             (format stream "~&~A" (make-string ind :initial-element #\Space))
             (cond
               (nested
                (let* ((lam (cadr form)))
                  (format stream "(closure (:lambda ~A" (%asm-lambda-list-text (cadr lam)))
                  (%asm-print-items (cddr lam) stream (+ ind 4))
                  (format stream "~%~A~D)" (make-string (+ ind 2) :initial-element #\Space) (caddr form))))
               (t
                (let ((text (%asm-form-text form)))
                  (write-string text stream)
                  (when lastp (write-string ")" stream))
                  (when comment-p
                    (format stream "~A; ~4,'0D" (if (< (+ ind (length text) (if lastp 1 0)) *asm-comment-column*)
                                                     (make-string (- *asm-comment-column* ind (length text) (if lastp 1 0)) :initial-element #\Space)
                                                     "  ")
                            (cadr e))))))))))
      nil)))

(defun %asm-print-items (items stream ind)
  "Print a nested routine's ITEMS (forms and label symbols) one per line."
  (let ((last (car (last items))))
    (dolist (it items)
      (cond ((symbolp it) (format stream "~&~A~A" (make-string (max 0 (- ind 2)) :initial-element #\Space) (%asm-sym-text it)))
            ((and (eq (car it) 'closure) (consp (cadr it)) (eq (car (cadr it)) :lambda))
             (format stream "~&~A(closure (:lambda ~A" (make-string ind :initial-element #\Space) (%asm-lambda-list-text (cadr (cadr it))))
             (%asm-print-items (cddr (cadr it)) stream (+ ind 4))
             (format stream "~%~A~D)" (make-string (+ ind 2) :initial-element #\Space) (caddr it)))
            (t (format stream "~&~A~A" (make-string ind :initial-element #\Space) (%asm-form-text it))))
      (when (eq it last) (write-string ")" stream)))))

(defun %asm-lambda-list-text (ll)
  (with-output-to-string (s)
    (write-char #\( s)
    (let ((first t))
      (dolist (x ll) (unless first (write-char #\Space s)) (setq first nil) (write-string (%asm-sym-text x) s)))
    (write-char #\) s)))

(defun asm-listing (function &optional stream &key (addresses t))
  "Print the assembly source of the compiled FUNCTION (or the name of one) as a DEFASM form, with each instruction's
address as a comment unless ADDRESSES is NIL.  Jump targets are labels L<address>.  The text can be edited and
evaluated again.  With STREAM NIL (the default) return the text as a string."
  (let ((out (or stream (make-string-output-stream))))
    (let* ((fn (if (symbolp function) (symbol-function function) function)))
      (unless (ignore-errors (%code-parts fn)) (error "ASM-LISTING: ~S is not a compiled function" function))
      (multiple-value-bind (ll entries parts unsafe nfree) (%asm-decode fn)
        (let* ((name (let ((n (nth 0 parts))) (or n (and (symbolp function) function) 'anonymous)))
               (doc (and (symbolp name) (get name '%doc-function))))
          (when (> nfree 0)
            (format out "~&; This function captures ~D variable~:P (fref); as a top-level form it cannot be assembled by itself:~%; use it as (closure (:lambda ...) ~D).~%" nfree nfree))
          (format out "~&(defasm ~A ~A" (%asm-sym-text name) (%asm-lambda-list-text ll))
          (when doc (format out "~%  ~S" doc))
          (when unsafe (format out "~%~A(:unsafe)" (make-string *asm-label-indent* :initial-element #\Space)))
          (if entries
              (%asm-print-entries entries out *asm-instruction-indent* addresses)
              (write-string ")" out))
          (terpri out))))
    (if stream nil (get-output-stream-string out))))
