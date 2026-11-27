;;;; 00-defs.lisp -- compiler data structures and opcode table.
;;;;
;;;; The compiler has two passes (see docs/design/compiler.md):
;;;;   ANALYZE  source form -> AST of lists (:const :lref :gref :call ...) with
;;;;            macros expanded, variables resolved to CVAR structures and free
;;;;            variables / assignments recorded.
;;;;   GENERATE AST -> bytecode for the VM in src/vm.c.

(in-package :cl)

(defstruct (cvar (:constructor make-cvar (name lam)))
  name lam (captured nil) (mutated nil) (slot 0)
  (type nil))     ; a ty (05-types.lisp): declared, or inferred for a variable that is never assigned

(defstruct (clam (:constructor make-clam (name parent)))
  name parent (nreq 0) (nopt 0) (rest nil) (params nil) (body nil) (free nil) (arglist nil)
  (policy nil)    ; policy changes from the lambda's own OPTIMIZE declarations
  (checks nil))   ; (cvar . ty) for each parameter whose declared type is checked on entry

(defstruct (cblock (:constructor make-cblock (name lam)))
  name lam label depth nspec (tailp nil))

(defstruct (ctb (:constructor make-ctb (lam)))
  lam depth nspec)

(defstruct (ctag (:constructor make-ctag (name)))
  name label)

(defstruct (aenv (:constructor make-aenv ()))
  (vars nil)      ; alist symbol -> cvar
  (funs nil)      ; alist name -> (:var cvar) | (:macro function)
  (blocks nil)    ; alist name -> cblock | (:token symbol)
  (tags nil)      ; alist tag  -> (ctb . ctag) | (:sm state restart index)
  (specials nil)) ; locally declared special symbols

(defparameter *opcodes*
  ;; (name operand-count) in the order of the OP_ enum in src/neogenera.h; each entry becomes (name code nargs)
  (let ((i -1))
    (mapcar (lambda (e) (setq i (+ i 1)) (list (car e) i (cadr e)))
            '((:const 1) (:lref 1) (:lset 1) (:fref 1) (:gref 1) (:gset 1) (:gfun 1) (:pop 0) (:dup 0) (:jmp 1)
              (:jnil 1) (:jnnil 1) (:call 1) (:tailcall 1) (:ret 0) (:closure 2) (:box 0) (:unbox 0) (:boxset 0)
              (:bind 1) (:unbind 1) (:slide 1) (:car 0) (:cdr 0) (:cons 0) (:eq 0) (:not 0) (:add 0) (:sub 0)
              (:lt 0) (:gt 0) (:le 0) (:ge 0) (:numeq 0) (:retv 0)
              ;; specialised by declarations (see 15-specialize.lisp)
              (:mul 0) (:addfx 0) (:subfx 0) (:mulfx 0) (:incfx 0) (:decfx 0) (:ltfx 0) (:gtfx 0) (:lefx 0)
              (:gefx 0) (:numeqfx 0) (:logandfx 0) (:logiorfx 0) (:logxorfx 0) (:remfx 0) (:modfx 0) (:ashfx 1)
              (:addfl 0) (:subfl 0) (:mulfl 0) (:divfl 0) (:ltfl 0) (:gtfl 0) (:lefl 0) (:gefl 0) (:numeqfl 0)
              (:fixtofl 0) (:carx 0) (:cdrx 0) (:svref 0) (:svrefx 0) (:svset 0) (:svsetx 0) (:schar 0)
              (:chkfix 0) (:chktype 1) (:chkrange 1)))))

(defun op-code (op) (cadr (assq op *opcodes*)))
(defun op-nargs (op) (caddr (assq op *opcodes*)))

(defun op-delta (op args)
  (case op
    ((:const :lref :fref :gref :gfun :dup) 1)
    ((:lset :gset :pop :jnil :jnnil :bind :ret :retv :cons :eq :add :sub :lt :gt :le :ge :numeq :boxset
      :mul :addfx :subfx :mulfx :ltfx :gtfx :lefx :gefx :numeqfx :logandfx :logiorfx :logxorfx :remfx :modfx
      :addfl :subfl :mulfl :divfl :ltfl :gtfl :lefl :gefl :numeqfl :svref :svrefx :schar) -1)
    ((:svset :svsetx) -2)
    ((:call :tailcall) (- (car args)))
    (:closure (- 1 (cadr args)))
    (:slide (- (car args)))
    (t 0)))

(defparameter *inline-ops*
  '((car :car 1) (cdr :cdr 1) (cons :cons 2) (eq :eq 2) (not :not 1) (null :not 1)
    (+ :add 2) (* :mul 2) (- :sub 2) (< :lt 2) (> :gt 2) (<= :le 2) (>= :ge 2) (= :numeq 2)))
