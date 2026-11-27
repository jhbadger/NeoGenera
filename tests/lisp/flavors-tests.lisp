;;;; flavors-tests.lisp -- Flavors: defflavor, methods, daemons, combinations, whoppers, wrappers, init protocol.

(in-package :cl)

;;; the Lisp Machine Manual's example
(defvar *default-x-velocity* 2.0)
(defflavor ship ((x-position 0.0) (y-position 0.0) (x-velocity *default-x-velocity*) (y-velocity 3.0) mass)
  ()
  :gettable-instance-variables :settable-instance-variables :inittable-instance-variables)

(defmethod (ship :speed) ()
  (sqrt (+ (* x-velocity x-velocity) (* y-velocity y-velocity))))

(defmethod (ship :advance) (dt)
  (setq x-position (+ x-position (* x-velocity dt)))
  (incf y-position (* y-velocity dt))
  (list x-position y-position))

(deftest flavors-basics
  (let ((s (make-instance 'ship :x-position 3.4)))
    (is-equal 3.4 (send s :x-position))
    (is-equal 0.0 (send s :y-position))
    (is-equal 2.0 (send s :x-velocity))
    (is (typep s 'ship))
    (is (flavor-instance-p s))
    (is (not (flavor-instance-p 3)))
    (is-equal 5.0 (send (make-instance 'ship :x-velocity 3.0 :y-velocity 4.0) :speed))
    (is-equal 4.0 (send s :set-y-position 4.0))
    (is-equal 4.0 (send s :y-position))
    (is-equal '(5.4 7.0) (send s :advance 1.0))      ; setq / incf of instance variables
    (is-equal 5.4 (send s :x-position))
    ;; mass has no initial value: unbound
    (signals-error (send s :mass))
    (send s :set-mass 7)
    (is-equal 7 (send s :mass))
    (is-equal 7 (lexpr-send s :mass nil))
    (is-equal 9 (send (make-instance 'ship :mass 9) :mass))))

(deftest flavors-unhandled-and-unknown
  (let ((s (make-instance 'ship)))
    (signals-error (send s :no-such-message))
    (is (eq :caught (handler-case (send s :no-such-message 1 2) (unclaimed-message (c) (if (eq (cell-error-name c) :no-such-message) :caught :wrong)))))
    (is-equal '(1 2) (handler-case (send s :zap 1 2) (unclaimed-message (c) (unclaimed-message-arguments c))))
    (is (not (operation-handled-p s :zap)))
    (is (operation-handled-p s :speed))
    (is (null (send-if-handles s :zap)))
    (is-equal 2.0 (send-if-handles s :x-velocity))
    (is (functionp (get-handler-for s :speed)))
    (is (null (get-handler-for s :zap)))
    ;; an init keyword the flavor does not know
    (signals-error (make-instance 'ship :colour :red))
    (is (flavor-allows-init-keyword-p 'ship :mass))
    (is (not (flavor-allows-init-keyword-p 'ship :colour)))
    (is (not (null (make-instance 'ship :colour :red :allow-other-keys t))))
    (signals-error (send 42 :foo))
    (is-equal '(:foo 1) (send (lambda (&rest a) a) :foo 1))))

;;; component order, daemons
(defvar *trace* nil)
(defun note (x) (setq *trace* (cons x *trace*)))
(defun trace-of (thunk) (setq *trace* nil) (funcall thunk) (reverse *trace*))

(defflavor base-f ((name "base")) () :gettable-instance-variables :inittable-instance-variables)
(defflavor mix-a () (base-f))
(defflavor mix-b () (base-f))
(defflavor top-f () (mix-a mix-b))

(defmethod (base-f :hello) () (note :base-primary) :base)
(defmethod (base-f :before :hello) () (note :base-before))
(defmethod (base-f :after :hello) () (note :base-after))
(defmethod (mix-a :before :hello) () (note :a-before))
(defmethod (mix-a :after :hello) () (note :a-after))
(defmethod (mix-b :before :hello) () (note :b-before))
(defmethod (mix-b :after :hello) () (note :b-after))
(defmethod (mix-b :hello) () (note :b-primary) :b)
(defmethod (top-f :before :hello) () (note :top-before))
(defmethod (top-f :after :hello) () (note :top-after))

(deftest flavors-order-and-daemons
  ;; a shared base flavor is kept at its last position, vanilla-flavor is last
  (is-equal '(top-f mix-a base-f mix-b vanilla-flavor) (flavor-components 'top-f))
  (is-equal '(base-f vanilla-flavor) (flavor-components 'base-f))
  (is-equal '(top-f mix-a base-f mix-b vanilla-flavor standard-object t)
            (mapcar #'class-name (class-precedence-list (find-class 'top-f))))
  (is (typep (make-instance 'top-f) 'base-f))
  (is (typep (make-instance 'top-f) 'mix-b))
  (is (not (typep (make-instance 'mix-a) 'mix-b)))
  (let ((o (make-instance 'top-f)))
    ;; befores most specific first, ONE primary (the most specific), afters least specific first
    (is-equal '(:top-before :a-before :base-before :b-before :base-primary :b-after :base-after :a-after :top-after)
              (trace-of (lambda () (is-equal :base (send o :hello)))))
    (is-equal "base" (send o :name))))

;;; :around, whoppers, wrappers
(defflavor fl-counter ((n 0)) () :gettable-instance-variables :settable-instance-variables)
(defmethod (fl-counter :bump) (&optional (by 1)) (incf n by))
(defmethod (fl-counter :around :bump) (cont mt original-args &optional (by 1))
  (note (list :around-in by (car original-args)))
  (let ((v (funcall-with-mapping-table cont mt :bump (* 10 by))))
    (note (list :around-out v))
    v))

(defwhopper (fl-counter :bump) (&optional (by 1))
  (note (list :whopper-in by))
  (let ((v (continue-whopper (+ by 1))))
    (note (list :whopper-out v))
    (* 2 v)))

(defflavor wrapped-counter () (fl-counter))
(defwrapper (wrapped-counter :set-n) ((v) . body)
  `(let ((v (max v 0))) ,@body))

(deftest flavors-around-whoppers-wrappers
  (let ((c (make-instance 'fl-counter)))
    ;; whopper (+1) -> around (*10) -> primary
    (is-equal '((:whopper-in 1) (:around-in 2 :bump) (:around-out 20) (:whopper-out 20))
              (trace-of (lambda () (is-equal 40 (send c :bump 1)))))
    (is-equal 20 (send c :n)))
  (let ((w (make-instance 'wrapped-counter)))
    (send w :set-n -5)
    (is-equal 0 (send w :n))
    (send w :set-n 5)
    (is-equal 5 (send w :n))
    (is (typep w 'fl-counter))))

;;; method combination options
(defflavor lister () () (:method-combination (:list :base-flavor-last :parts) (:progn :base-flavor-first :setup)
                                              (:or :base-flavor-last :find) (:and :base-flavor-last :check)
                                              (:append :base-flavor-last :names) (:max :base-flavor-last :size)))
(defflavor part-1 () (lister))
(defflavor part-2 () (part-1))
(defmethod (lister :list :parts) () 'lister)
(defmethod (part-1 :list :parts) () 'part-1)
(defmethod (part-2 :list :parts) () 'part-2)
(defmethod (lister :progn :setup) () (note :lister-setup) :l)
(defmethod (part-2 :progn :setup) () (note :part-2-setup) :p2)
(defmethod (lister :or :find) () nil)
(defmethod (part-1 :or :find) () :found-in-part-1)
(defmethod (part-2 :or :find) () nil)
(defmethod (lister :and :check) () 1)
(defmethod (part-2 :and :check) () 2)
(defmethod (part-1 :and :check) () nil)
(defmethod (lister :append :names) () (list 'a))
(defmethod (part-2 :append :names) () (list 'b 'c))
(defmethod (lister :max :size) () 3)
(defmethod (part-2 :max :size) () 9)
(defmethod (part-1 :max :size) () 5)

(defflavor untyped () ())
(defmethod (untyped :or :thing) () 1)

(deftest flavors-method-combination
  (let ((o (make-instance 'part-2)))
    (is-equal '(part-2 part-1 lister) (send o :parts))
    (is-equal '(:lister-setup :part-2-setup) (trace-of (lambda () (is-equal :p2 (send o :setup)))))
    (is-equal :found-in-part-1 (send o :find))
    (is-equal nil (send o :check))
    (is-equal '(b c a) (send o :names))
    (is-equal 9 (send o :size)))
  (is-equal '(lister) (send (make-instance 'lister) :parts))
  (signals-error (eval '(defflavor bad () () (:method-combination (:nonsense :x)))))
  ;; a typed method on a message with no combination declared is an error when the message is sent
  (signals-error (send (make-instance 'untyped) :thing)))

;;; init protocol: :init, :after :init, init keywords, default init plist, required things
(defflavor widget ((label "none") size) () :gettable-instance-variables
  (:inittable-instance-variables label)
  (:init-keywords :scale)
  (:default-init-plist :scale 2))
(defmethod (widget :after :init) (plist)
  (setq size (* (getf plist :scale) 10))
  (note (list :init (getf plist :scale))))
(defmethod (widget :size) () size)

(defflavor needs-things () () (:required-instance-variables label) (:required-methods :size) (:required-init-keywords :id))
(defflavor needy-widget () (widget needs-things))
(defflavor needy-bad () (needs-things))

(deftest flavors-init
  (is-equal '((:init 2)) (trace-of (lambda () (make-instance 'widget))))
  (is-equal 20 (send (make-instance 'widget) :size))
  (is-equal 50 (send (make-instance 'widget :scale 5 :label "x") :size))
  (is-equal "x" (send (make-instance 'widget :label "x") :label))
  (signals-error (make-instance 'widget :size 3))                ; size is not inittable
  (is (not (null (make-instance 'needy-widget :id 1))))
  (signals-error (make-instance 'needy-widget))                  ; required init keyword
  (signals-error (make-instance 'needy-bad :id 1))               ; required variable and method missing
  (is-equal 70 (send (instantiate-flavor 'widget '(:scale 7)) :size)))

;;; :default-handler, instance variables visible in functions, vanilla messages
(defun fallback (message &rest args) (list :fallback message args (flavor-instance-p self)))
(defflavor lenient () () (:default-handler fallback))
(defflavor point-f ((px 1) (py 2)) () :gettable-instance-variables)
(defun point-sum () (declare-flavor-instance-variables (point-f) (+ px py)))
(defmethod (point-f :sum) () (point-sum))
(defmethod (point-f :self-check) () (list (eq self (send self :self-itself)) (funcall (lambda () (eq self self)))))
(defmethod (point-f :self-itself) () self)
(defmethod (point-f :shadow) (px) (list px py))               ; a parameter shadows an instance variable

(deftest flavors-misc
  (is-equal '(:fallback :anything (1 2) t) (send (make-instance 'lenient) :anything 1 2))
  (let ((p (make-instance 'point-f)))
    (is-equal 3 (send p :sum))
    (is-equal '(t t) (send p :self-check))
    (is-equal '(9 2) (send p :shadow 9))
    (is-equal 3 (send p :eval-inside-yourself '(+ px py)))
    (is-equal 5 (send p :funcall-inside-yourself #'+ 2 3))
    (is-equal '(:self-itself :sum) (intersection '(:self-itself :sum :zzz) (send p :which-operations)))
    (is (send p :operation-handled-p :sum))
    (is (not (send p :operation-handled-p :nope)))
    (is-equal 3 (send p :send-if-handles :sum))
    (is-equal "#<POINT-F>" (prin1-to-string p))
    (is (functionp (send p :get-handler-for :sum)))
    (is-equal '(1 2) (let ((s (make-string-output-stream)))
                       (describe p s)
                       (let ((txt (get-output-stream-string s)))
                         (list (if (search "an object of flavor POINT-F" txt) 1 0) (if (search "PX: 1" txt) 2 0)))))))

(defflavor custom-print ((v 1)) () (:inittable-instance-variables v))
(defmethod (custom-print :print-self) (stream depth slashify)
  (declare (ignore depth slashify))
  (format stream "<custom ~A>" v))

(defflavor lonely () () :no-vanilla-flavor)

(deftest flavors-vanilla-and-print
  (is-equal "<custom 5>" (prin1-to-string (make-instance 'custom-print :v 5)))
  (is-equal '(lonely) (flavor-components 'lonely))
  (is (not (typep (make-instance 'lonely) 'vanilla-flavor)))
  (signals-error (send (make-instance 'lonely) :which-operations)))

;;; redefinition and removal
(defflavor redef ((a 1)) () :gettable-instance-variables)
(defmethod (redef :hi) () :one)
(defflavor dep () (redef))

(deftest flavors-redefinition
  (let ((o (make-instance 'redef)))
    (is-equal :one (send o :hi))
    (defmethod (redef :hi) () :two)
    (is-equal :two (send o :hi))
    (undefmethod (redef :hi))
    (signals-error (send o :hi))
    ;; adding an instance variable and a method to a flavor updates its existing instances
    (eval '(defflavor redef ((a 1) (b 5)) () :gettable-instance-variables))
    (is-equal 5 (send o :b))
    (is-equal 1 (send o :a))
    ;; the user's own method is not replaced by a generated one when the flavor is redefined
    (defmethod (redef :a) () :mine)
    (eval '(defflavor redef ((a 1) (b 5)) () :gettable-instance-variables))
    (is-equal :mine (send o :a))
    ;; a flavor that is a component changes its dependents
    (let ((d (make-instance 'dep)))
      (is-equal 5 (send d :b))
      (eval '(defflavor redef ((a 1) (b 8)) () :gettable-instance-variables))
      (is-equal 5 (send d :b))   ; existing instance keeps its value
      (is-equal 8 (send (make-instance 'dep) :b)))))

;;; included flavors and CLOS coexistence
(defflavor needs-base () () (:included-flavors base-f))
(defflavor uses-needs () (needs-base))

(defclass plain-clos () ((q :initform 1 :accessor plain-q)))
(defmethod describe-it ((p plain-clos)) (list :clos (plain-q p)))
(defmethod (setf plain-q-twice) (v (p plain-clos)) (setf (plain-q p) (* 2 v)))

(deftest flavors-included-and-clos
  (is-equal '(needs-base base-f vanilla-flavor) (flavor-components 'needs-base))
  (is-equal '(uses-needs needs-base base-f vanilla-flavor) (flavor-components 'uses-needs))
  (is-equal "base" (send (make-instance 'uses-needs) :name))
  ;; ordinary CLOS is unaffected by the new DEFMETHOD
  (is-equal '(:clos 1) (describe-it (make-instance 'plain-clos)))
  (let ((p (make-instance 'plain-clos))) (setf (plain-q-twice p) 4) (is-equal 8 (plain-q p)))
  (signals-error (send (make-instance 'plain-clos) :foo))
  ;; a generic function can specialize on a flavor, and flavor instances are CLOS instances
  (defgeneric flavor-gf (x))
  (defmethod flavor-gf ((x ship)) :a-ship)
  (is-equal :a-ship (flavor-gf (make-instance 'ship)))
  (is (eq (find-class 'ship) (class-of (make-instance 'ship))))
  (is (stringp (with-output-to-string (s) (describe-flavor 'ship s)))))

(deftest flavors-definition-sections
  ;; the editor and source-location tools recognise flavor definitions
  (is-equal '("flavor ship" "method ship" "function foo")
            (mapcar #'section-label
                    (text-sections (format nil "(defflavor ship (a) ())~%~%(defmethod (ship :speed) ()~%  a)~%~%(defun foo () 1)~%")))))

;;; the Lisp Machine Manual's component-order example: flavor-1 (flavor-2 flavor-3), flavor-2 (flavor-4 flavor-5), flavor-3 (flavor-4)
(defflavor lm-4 () ())
(defflavor lm-5 () ())
(defflavor lm-2 () (lm-4 lm-5))
(defflavor lm-3 () (lm-4))
(defflavor lm-1 () (lm-2 lm-3))
(defflavor inc-base () ())
(defflavor inc-x () () (:included-flavors inc-base))
(defflavor inc-y () ())
(defflavor inc-z () (inc-x inc-y))
(defflavor inc-w () (inc-base inc-x))

(deftest flavors-manual-order
  (is-equal '(lm-1 lm-2 lm-4 lm-5 lm-3 vanilla-flavor) (flavor-components 'lm-1))
  ;; an included flavor goes right after the last flavor that includes it, unless it is a component itself
  (is-equal '(inc-z inc-x inc-base inc-y vanilla-flavor) (flavor-components 'inc-z))
  (is-equal '(inc-w inc-base inc-x vanilla-flavor) (flavor-components 'inc-w)))

;;; the manual's :set, :case, :abstract-flavor, :daemon-with-or/and/override, :inverse-around, wrapper with a bare argument list
(defflavor setty ((a 1) (b 2)) () :settable-instance-variables)
(defflavor abs-f () () :abstract-flavor)
(defflavor casey (a b) ()
  (:method-combination (:case :base-flavor-last :win)))
(defmethod (casey :case :win :a) () 'got-a)
(defmethod (casey :case :win :sum) () 'sum)
(defmethod (casey :win) (sub) (list :other sub))
(defflavor dor () ()
  (:method-combination (:daemon-with-or :base-flavor-last :f) (:daemon-with-and :base-flavor-last :g)
                       (:daemon-with-override :base-flavor-last :h)))
(defflavor dor-sub () (dor))
(defmethod (dor :before :f) () (note :before))
(defmethod (dor :f) () (note :primary) :primary)
(defmethod (dor-sub :or :f) () (note :or) nil)
(defmethod (dor :after :f) () (note :after))
(defmethod (dor :g) () :g-primary)
(defmethod (dor-sub :and :g) () 1)
(defmethod (dor :h) () :h-primary)
(defmethod (dor-sub :override :h) () nil)
(defflavor dor-sub2 () (dor-sub))
(defmethod (dor-sub2 :or :f) () :shortcut)
(defmethod (dor-sub2 :and :g) () nil)
(defmethod (dor-sub2 :override :h) () :overridden)
(defflavor inv () ())
(defmethod (inv :inverse-around :zip) (cont mt orig) (note :inv-in) (prog1 (funcall-with-mapping-table cont mt :zip) (note :inv-out)))
(defmethod (inv :around :zip) (cont mt orig) (note :around-in) (prog1 (funcall-with-mapping-table cont mt :zip) (note :around-out)))
(defmethod (inv :zip) () (note :zip) 7)
(defflavor wrapq () ())
(defmethod (wrapq :tag) (&rest args) (list :tag args))
(defwrapper (wrapq :tag) (all-args . body) `(cons :wrapped (progn ,@body)))

(deftest flavors-more-combinations
  (let ((s (make-instance 'setty)))
    (send s :set :a 10)
    (is-equal 10 (send s :a))
    (signals-error (make-instance 'abs-f))
    (is-equal 'got-a (send (make-instance 'casey) :win :a))
    (is-equal 'sum (send (make-instance 'casey) :win :sum))
    (is-equal '(:other :zzz) (send (make-instance 'casey) :win :zzz)))
  (let ((d (make-instance 'dor-sub)) (e (make-instance 'dor-sub2)))
    (is-equal '(:before :or :primary :after) (trace-of (lambda () (is-equal :primary (send d :f)))))
    (is-equal :shortcut (send e :f))
    (is-equal :g-primary (send d :g))
    (is-equal nil (send e :g))
    (is-equal :h-primary (send d :h))
    (is-equal :overridden (send e :h)))
  (is-equal '(:inv-in :around-in :zip :around-out :inv-out) (trace-of (lambda () (is-equal 7 (send (make-instance 'inv) :zip)))))
  (is-equal '(:wrapped :tag (1 2)) (send (make-instance 'wrapq) :tag 1 2))
  (signals-error (eval '(defflavor bad-style () () (:method-combination (:bogus-style :base-flavor-last :x))))))

;;; method named by a function, Lisp Machine utilities, property-list-mixin
(defun wide-handler (op x) (list op x self))
(defflavor fh () ())
(defmethod (fh :wide) wide-handler)
(defflavor pl () (property-list-mixin))

(deftest flavors-utilities
  (let ((o (make-instance 'fh)))
    (is-equal (list :wide 3 o) (send o :wide 3)))
  (is (instancep (make-instance 'fh)))
  (is (not (instancep 5)))
  (let ((p (make-instance 'point-f)))
    (is-equal 1 (symeval-in-instance p 'px))
    (set-in-instance p 'px 9)
    (is-equal 9 (send p :px))
    (is (null (symeval-in-instance p 'nope t)))
    (signals-error (symeval-in-instance p 'nope)))
  (is (memq 'ship *all-flavor-names*))
  (let ((o (make-instance 'pl :property-list '(:a 1))))
    (is-equal 1 (send o :get :a))
    (is-equal 'dflt (send o :get :zz 'dflt))
    (send o :putprop 5 :b)
    (is-equal 5 (send o :get :b))
    (is-equal '(:b 5 :a 1) (send o :getl '(:b)))
    (send o :push-property 'x :lst)
    (send o :push-property 'y :lst)
    (is-equal '(y x) (send o :get :lst))
    (is-equal '(1) (send o :remprop :a))
    (is (null (send o :get :a)))
    (is-equal '(:lst (y x) :b 5) (send o :property-list)))
  (defflavor to-remove () ())
  (undefflavor 'to-remove)
  (signals-error (make-instance 'to-remove)))
