;;;; 30-struct.lisp -- DEFSTRUCT and hash tables.

(in-package :cl)

(defun %define-struct (name slot-specs parent)
  ;; slot-specs: list of (slot-name default-form)
  (let ((desc (cons name (mapcar #'car slot-specs))))
    (%put name '%struct-desc desc)
    (%put name '%struct-slots slot-specs)
    (when parent (%put name '%struct-parent parent))
    name))

(defun %opt-value (options key default)
  (dolist (o options default)
    (cond ((eq o key) (return default))
          ((and (consp o) (eq (car o) key)) (return (if (cdr o) (cadr o) default))))))
(defun %opt-present (options key)
  (dolist (o options nil)
    (when (or (eq o key) (and (consp o) (eq (car o) key))) (return t))))

(defun %sym-concat (&rest parts)
  (intern (apply #'%string-concat (mapcar #'string parts))))

(defmacro defstruct (name-and-options &rest slot-specs)
  (let* ((name (if (consp name-and-options) (car name-and-options) name-and-options))
         (options (if (consp name-and-options) (cdr name-and-options) nil))
         (conc (let ((c (if (%opt-present options :conc-name)
                            (%opt-value options :conc-name nil)
                            (%string-concat (symbol-name name) "-"))))
                 (if c (string c) "")))
         (include (dolist (o options nil) (when (and (consp o) (eq (car o) :include)) (return o))))
         (parent (and include (cadr include)))
         (own (mapcar (lambda (s) (if (consp s) (list (car s) (cadr s)) (list s nil)))
                      (if (stringp (car slot-specs)) (cdr slot-specs) slot-specs)))
         (inherited (if parent (get parent '%struct-slots) nil))
         (overrides (and include (cddr include)))
         (all (append (mapcar (lambda (s)
                                (let ((o (find (car s) overrides :key (lambda (x) (if (consp x) (car x) x)))))
                                  (if (consp o) (list (car s) (cadr o)) s)))
                              inherited)
                      own))
         (names (mapcar #'car all))
         (predicate (let ((p (assoc :predicate (remove-if-not #'consp options))))
                      (cond ((and p (null (cdr p))) nil)
                            (p (cadr p))
                            (t (%sym-concat name "-P")))))
         (copier (let ((p (assoc :copier (remove-if-not #'consp options))))
                   (cond ((and p (null (cadr p))) nil)
                         (p (cadr p))
                         (t (%sym-concat "COPY-" name)))))
         (ctor-opts (remove-if-not (lambda (o) (and (consp o) (eq (car o) :constructor))) options))
         (forms nil))
    (setq forms (list `(%define-struct ',name ',all ',parent)))
    ;; constructors
    (when (or (null ctor-opts) (some (lambda (o) (cadr o)) ctor-opts))
      (let ((specs (if ctor-opts (remove-if-not #'cadr ctor-opts) (list (list :constructor (%sym-concat "MAKE-" name))))))
        (dolist (c specs)
          (if (cddr c)
              ;; BOA constructor
              (let* ((ll (caddr c))
                     (vars (%ll-vars ll)))
                (setq forms (cons `(defun ,(cadr c) ,ll
                                     (%make-struct (get ',name '%struct-desc)
                                                   ,@(mapcar (lambda (s) (if (member (car s) vars) (car s) (cadr s))) all)))
                                  forms)))
              (setq forms (cons `(defun ,(cadr c) (&key ,@(mapcar (lambda (s) (list (car s) (cadr s))) all))
                                   (%make-struct (get ',name '%struct-desc) ,@names))
                                forms))))))
    (when predicate
      (setq forms (cons `(defun ,predicate (x) (%struct-typep x ',name)) forms)))
    (when copier
      (setq forms (cons `(defun ,copier (x) (%copy-struct x)) forms)))
    (let ((i 0))
      (dolist (s all)
        (setq i (+ i 1))
        (let ((acc (%sym-concat conc (car s))) (setter (%sym-concat "%SET-" conc (car s))))
          (setq forms (cons `(defun ,acc (x) (%struct-ref-typed x ,i ',name)) forms))
          (setq forms (cons `(defun ,setter (x v) (%struct-set-typed x ,i ',name v)) forms))
          (setq forms (cons `(%def-setf ',acc ',setter) forms))
          (setq forms (cons `(%put ',acc '%struct-accessor '(,i ,name)) forms)))))
    `(progn ,@(nreverse forms) ',name)))

(defun %ll-vars (ll)
  (let ((acc nil))
    (dolist (x ll)
      (cond ((member x '(&optional &rest &key &aux &allow-other-keys &body)) nil)
            ((consp x) (setq acc (cons (car x) acc)))
            (t (setq acc (cons x acc)))))
    acc))

(defun copy-structure (x) (%copy-struct x))

;;; ---- hash tables ------------------------------------------------------------
;;; A bucket is a list of entries.  In an ordinary table an entry is a cons (key . value); in a weak table it is a
;;; weak object (%MAKE-WEAK mode key value) that the collector breaks when its key or value is collected, according
;;; to the table's weakness.  Broken entries are dropped as they are met.

(defstruct (hash-table (:constructor %make-ht (test buckets)) (:predicate %hash-table-p)
                       (:conc-name %ht-) (:copier nil))
  test (count 0) buckets (weakness nil))

(defun hash-table-p (x) (%hash-table-p x))

(defun %weakness-mode (weakness)
  (case weakness
    (:key 1) (:value 2) (:key-and-value 3) (:key-or-value 4)
    (t (error "~S is not a hash table weakness (NIL, :KEY, :VALUE, :KEY-AND-VALUE or :KEY-OR-VALUE)" weakness))))

(defun make-hash-table (&key (test 'eql) (size 16) weakness &allow-other-keys)
  "A new hash table.  WEAKNESS, if not NIL, lets the collector remove an entry: :KEY when nothing else holds its
key, :VALUE when nothing else holds its value, :KEY-AND-VALUE when either is unreferenced, :KEY-OR-VALUE when
both are."
  (let* ((tst (cond ((functionp test) (%function-name test)) (t test)))
         (h (%make-ht tst (%make-vector (max 8 size) nil))))
    (when weakness (%weakness-mode weakness) (setf (%ht-weakness h) weakness))
    h))

(defun hash-table-weakness (h) "The weakness of H: NIL, :KEY, :VALUE, :KEY-AND-VALUE or :KEY-OR-VALUE." (%ht-weakness h))

(defun %ht-hash (h key)
  (let ((test (%ht-test h)))
    (if (and (eq test 'equalp) (stringp key))
        (sxhash (string-upcase key))
        (sxhash key))))

(defun %ht-index (h key) (mod (%ht-hash h key) (length (%ht-buckets h))))

(defun %ht-key= (test a b)
  (cond ((eq test 'eq) (eq a b)) ((eq test 'eql) (eql a b)) ((eq test 'equal) (equal a b)) (t (equalp a b))))

(defun %weak-live-p (e) (< (%weak-mode e) 6))

(defun %wht-prune (h idx)
  "Drop the broken entries of bucket IDX of the weak table H; returns the bucket."
  (let ((bucket (aref (%ht-buckets h) idx)))
    (if (dolist (e bucket t) (unless (%weak-live-p e) (return nil)))
        bucket
        (let ((live (remove-if-not #'%weak-live-p bucket)))
          (setf (%ht-count h) (- (%ht-count h) (- (length bucket) (length live))))
          (%aset (%ht-buckets h) idx live)
          live))))

(defun %ht-find (h key)
  (let ((test (%ht-test h)))
    (if (%ht-weakness h)
        (dolist (e (%wht-prune h (%ht-index h key)) nil)
          (when (%ht-key= test (%weak-key e) key) (return e)))
        (let ((bucket (aref (%ht-buckets h) (%ht-index h key))))
          (cond ((eq test 'eq) (assq key bucket))
                ((eq test 'eql) (assql key bucket))
                ((eq test 'equal) (%assoc-equal key bucket))
                (t (dolist (e bucket nil) (when (equalp (car e) key) (return e)))))))))

(defun gethash (key h &optional default)
  (let ((e (%ht-find h key)))
    (cond ((null e) (values default nil))
          ((%ht-weakness h) (values (%weak-value e) t))
          (t (values (cdr e) t)))))

(defun %ht-entry-key (h e) (if (%ht-weakness h) (%weak-key e) (car e)))

(defun %grow-ht (h)
  (let* ((old (%ht-buckets h)) (n (* 2 (length old))) (new (%make-vector n nil)) (weak (%ht-weakness h)) (count 0))
    (dotimes (i (length old))
      (dolist (e (aref old i))
        (when (or (not weak) (%weak-live-p e))
          (let ((idx (mod (%ht-hash h (%ht-entry-key h e)) n)))
            (setq count (+ count 1))
            (%aset new idx (cons e (aref new idx)))))))
    (setf (%ht-buckets h) new (%ht-count h) count)))

(defun %sethash (key h value)
  (let ((e (%ht-find h key)) (weak (%ht-weakness h)))
    (cond ((null e)
           (when (> (%ht-count h) (* 2 (length (%ht-buckets h)))) (%grow-ht h))
           (let ((idx (%ht-index h key)))
             (%aset (%ht-buckets h) idx (cons (if weak (%make-weak (%weakness-mode weak) key value) (cons key value))
                                              (aref (%ht-buckets h) idx))))
           (setf (%ht-count h) (+ 1 (%ht-count h))))
          (weak (%weak-set-value e value))
          (t (rplacd e value)))
    value))
(%def-setf 'gethash '%sethash)

(defun remhash (key h)
  (let* ((idx (%ht-index h key)) (e (%ht-find h key)))
    (when e
      (%aset (%ht-buckets h) idx (remove e (aref (%ht-buckets h) idx)))
      (when (%ht-weakness h) (%weak-break e))
      (setf (%ht-count h) (- (%ht-count h) 1))
      t)))
(defun clrhash (h)
  (setf (%ht-buckets h) (%make-vector 8 nil))
  (setf (%ht-count h) 0)
  h)
(defun hash-table-count (h)
  (when (%ht-weakness h) (dotimes (i (length (%ht-buckets h))) (%wht-prune h i)))
  (%ht-count h))
(defun hash-table-test (h) (%ht-test h))
(defun %ht-pairs (h)
  "The entries of H as (key . value) conses: H's own entries, or for a weak table fresh conses of the live ones."
  (let ((acc nil) (weak (%ht-weakness h)))
    (dotimes (i (length (%ht-buckets h)))
      (dolist (e (if weak (%wht-prune h i) (aref (%ht-buckets h) i)))
        (setq acc (cons (if weak (cons (%weak-key e) (%weak-value e)) e) acc))))
    (nreverse acc)))
(defun maphash (fn h)
  (dolist (e (%ht-pairs h)) (funcall fn (car e) (cdr e)))
  nil)
(defun hash-table-keys (h) (mapcar #'car (%ht-pairs h)))
(defun hash-table-values (h) (mapcar #'cdr (%ht-pairs h)))

;;; ---- weak pointers ------------------------------------------------------------

(defun make-weak-pointer (object)
  "A weak pointer to OBJECT: it does not keep OBJECT from being collected."
  (%make-weak 0 object nil))
(defun weak-pointer-p (x) "True if X is a weak pointer." (and (%weakp x) (member (%weak-mode x) '(0 7)) t))
(defun weak-pointer-value (wp)
  "(values object t) while the object WP points to is alive; (values nil nil) once it has been collected."
  (unless (weak-pointer-p wp) (error 'type-error :datum wp :expected-type 'weak-pointer))
  (if (eql (%weak-mode wp) 0) (values (%weak-key wp) t) (values nil nil)))

;;; ---- property list utilities -------------------------------------------------
(defun remprop (sym ind)
  (let ((pl (symbol-plist sym)) (acc nil) (found nil))
    (do-list-tails (l pl)
      (if (eq (car l) ind) (setq found t) (setq acc (cons (cadr l) (cons (car l) acc))))
      (setq l (cdr l)))
    (when found (%set-symbol-plist sym (nreverse acc)))
    found))
(defun copy-alist (a) (mapcar (lambda (e) (if (consp e) (cons (car e) (cdr e)) e)) a))
(defun constantly (v) (lambda (&rest r) (declare (ignore r)) v))
(defun complement (f) (lambda (&rest args) (not (apply f args))))
(defun fdefinition (n) (symbol-function n))
