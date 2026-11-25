;;;; 00-defs.lisp -- compiler data structures and opcode table.
;;;;
;;;; The compiler has two passes (see docs/design/compiler.md):
;;;;   ANALYZE  source form -> AST of lists (:const :lref :gref :call ...) with
;;;;            macros expanded, variables resolved to CVAR structures and free
;;;;            variables / assignments recorded.
;;;;   GENERATE AST -> bytecode for the VM in src/vm.c.

(in-package :cl)

(defstruct (cvar (:constructor make-cvar (name lam)))
  name lam (captured nil) (mutated nil) (slot 0))

(defstruct (clam (:constructor make-clam (name parent)))
  name parent (nreq 0) (nopt 0) (rest nil) (params nil) (body nil) (free nil))

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
  '((:const 0 1) (:lref 1 1) (:lset 2 1) (:fref 3 1) (:gref 4 1) (:gset 5 1)
    (:gfun 6 1) (:pop 7 0) (:dup 8 0) (:jmp 9 1) (:jnil 10 1) (:jnnil 11 1)
    (:call 12 1) (:tailcall 13 1) (:ret 14 0) (:closure 15 2) (:box 16 0)
    (:unbox 17 0) (:boxset 18 0) (:bind 19 1) (:unbind 20 1) (:slide 21 1)
    (:car 22 0) (:cdr 23 0) (:cons 24 0) (:eq 25 0) (:not 26 0) (:add 27 0)
    (:sub 28 0) (:lt 29 0) (:gt 30 0) (:le 31 0) (:ge 32 0) (:numeq 33 0) (:retv 34 0)))

(defun op-code (op) (cadr (assq op *opcodes*)))
(defun op-nargs (op) (caddr (assq op *opcodes*)))

(defun op-delta (op args)
  (case op
    ((:const :lref :fref :gref :gfun :dup) 1)
    ((:lset :gset :pop :jnil :jnnil :bind :ret :retv :cons :eq :add :sub :lt :gt :le :ge :numeq :boxset) -1)
    ((:call :tailcall) (- (car args)))
    (:closure (- 1 (cadr args)))
    (:slide (- (car args)))
    (t 0)))

(defparameter *inline-ops*
  '((car :car 1) (cdr :cdr 1) (cons :cons 2) (eq :eq 2) (not :not 1) (null :not 1)
    (+ :add 2) (- :sub 2) (< :lt 2) (> :gt 2) (<= :le 2) (>= :ge 2) (= :numeq 2)))
