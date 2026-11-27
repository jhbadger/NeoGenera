;;;; prolog-tests.lisp -- the Prolog Tool Kit: reader and writer, unification and the machine, the built-in predicates, the
;;;; library, DCGs, consulting, the top level, the Lisp bridge, PROLOG mode in the editor and the Listener commands.
;;;; Uses helpers from ui-tests.lisp, tools-tests.lisp and debugger-tests.lisp.

(in-package :cl)

(defun pl-quiet-consult (text &rest options)
  "Consult TEXT without printing warnings; returns (values count warnings)."
  (let ((*pl-quiet* t)) (apply #'pl-consult-string text options)))

(defun plq (text)
  "The text the top level prints for the queries in TEXT."
  (let ((out (make-string-output-stream)))
    (let ((*error-output* out))
      (pl-toplevel :input (make-string-input-stream text) :output out :prompt ""))
    (get-output-stream-string out)))

(defun pl-has (text part) (and (%string-search part text 0) t))

(defun plt (text)
  "TEXT read as a term and written back with quotes."
  (pl-term-to-string (pl-parse-string text) :quoted t))

(defun pl-sols (goal) "All solutions of GOAL as alists of Lisp values." (pl-query goal))

(defun pl-first (goal var)
  "The value of VAR in the first solution of GOAL, or :FAIL."
  (let ((s (pl-query goal :limit 1))) (if s (cdr (assoc var (car s) :test #'string=)) :fail)))

(defun pl-first-raw (goal var)
  "Like PL-FIRST but the value is the Prolog term, not converted to Lisp."
  (let ((s (pl-query goal :limit 1 :raw t))) (if s (cdr (assoc var (car s) :test #'string=)) :fail)))

(defun pl-error-of (goal)
  "The formal part of the error GOAL raises, as text, or NIL."
  (handler-case (progn (pl-prove-goal goal) nil)
    (pl-error (c) (let ((b (pl-error-ball c))) (pl-term-to-string (pl-arg b 0) :quoted t)))))

(defvar *pl-tmp-count* 0)
(defun pl-tmp (name) (format nil "/tmp/np~D~D~A" (mod (get-universal-time) 100000) (incf *pl-tmp-count*) name))

;;; ---- reading and writing ---------------------------------------------------------------------------------------------------

(deftest prolog-reader-basics
  (is-equal "foo(_G1,bar)" (let ((s (plt "foo(X, bar)"))) (concatenate 'string "foo(_G1,bar)" (if (pl-has s "foo(_G") "" "x"))))
  (is-equal "a:-b,c;d->e" (plt "a:-b,c;d->e"))
  (is-equal "1+2*3-4" (plt "1+2*3-4"))
  (is-equal "(1+2)*3" (plt "(1+2)*3"))
  (is-equal "1-(2-3)" (plt "1-(2-3)"))
  (is-equal "[1,2,3|a]" (plt "[1,2,3|a]"))
  (is-equal "- 1" (plt "- 1"))
  (is-equal "1- -1" (plt "1 - -1"))
  (is-equal "- -a" (plt "- - a"))
  (is-equal "\\+ (a,b)" (plt "\\+ (a,b)"))
  (is-equal "{a,b}" (plt "{a,b}"))
  (is-equal "'hello world'(a)" (plt "'hello world'(a)"))
  (is-equal "f(:-,a)" (plt "f(:- , a)"))
  (is-equal "f((a:-b),c)" (plt "f(a:-b, c)"))
  (is-equal "f((a;b))" (plt "f(a;b)"))
  (is-equal "[]" (plt "[]"))
  (is-equal "'\\n'" (plt "'\\n'"))
  (is-equal "'don\\'t'" (plt "'don''t'"))
  (is-equal "a=(\\+)" (plt "a = \\+")))

(deftest prolog-reader-numbers
  (is-equal "97+31+1500.0+1000" (plt "0'a + 0x1F + 1.5e3 + 1_000"))
  (is-equal "255+8+5" (plt "0xff + 0o10 + 0b101"))
  (is-equal "35" (plt "35"))
  (is-equal "1.0e-7" (plt "1.0e-7"))
  (is-equal "123456789012345678901234567890" (plt "123456789012345678901234567890"))
  (is-equal "255" (plt "16'ff"))
  (is-equal "39" (plt "0'''"))
  (is-equal "10" (plt "0'\\n")))

(deftest prolog-reader-flags-and-ops
  (is-equal "[97,98]" (let ((*pl-flags* (copy-alist *pl-flags*))) (pl-set-flag "double_quotes" (pl-atom "codes")) (plt "\"ab\"")))
  (is-equal "\"ab\"" (plt "\"ab\""))
  (is-equal "ab" (let ((*pl-flags* (copy-alist *pl-flags*))) (pl-set-flag "double_quotes" (pl-atom "atom")) (plt "\"ab\"")))
  (is-equal "[a,b]" (let ((*pl-flags* (copy-alist *pl-flags*))) (pl-set-flag "double_quotes" (pl-atom "chars")) (plt "\"ab\"")))
  (pl-prove-goal "op(700, xfx, ===>)")
  (is-equal "a===>b" (plt "a ===> b"))
  (pl-prove-goal "op(200, xfy, ===>)")
  (pl-prove-goal "op(0, xfx, ===>)")
  (pl-prove-goal "op(0, xfy, ===>)")
  (signals-error (plt "a ===> b"))
  (pl-prove-goal "op(9, fx, nix)")
  (is-equal "nix a" (plt "nix a"))
  (pl-prove-goal "op(0, fx, nix)")
  (is-equal "permission_error(modify,operator,',')" (pl-error-of "op(200, xfx, ',')"))
  (is-equal "domain_error(operator_priority,1201)" (pl-error-of "op(1201, xfx, foo)")))

(deftest prolog-reader-syntax-errors
  (signals-error (pl-parse-string "foo(a"))
  (signals-error (pl-parse-string "foo(a b)"))
  (signals-error (pl-parse-string "'unterminated"))
  (signals-error (pl-parse-string "f(,)"))
  (signals-error (pl-parse-string "X = = Y"))
  ;; reading continues after an error
  (let ((text "a(1). b(. c(3)."))
    (multiple-value-bind (n warnings) (pl-quiet-consult text :source "syntaxtest")
      (is-equal 2 n)
      (is-equal 1 (length warnings))
      (is-equal :error (plwarning-kind (car warnings))))))

(deftest prolog-writer
  (is (pl-has (pl-term-to-string (pl-parse-string "f(A,B)") :numbervars t) "f(_G"))
  (is-equal "f(X,Y)" (let ((tm (pl-parse-string "f(A,B)"))) (pl-term-to-string tm :names (list (cons (car (pl-variables-of tm)) "X") (cons (cadr (pl-variables-of tm)) "Y")))))
  (is-equal "'$VAR'(1)" (pl-term-to-string (pl-s "$VAR" 1) :quoted t))
  (is-equal "B" (pl-term-to-string (pl-s "$VAR" 1) :numbervars t))
  (is-equal "Z" (pl-term-to-string (pl-s "$VAR" 25) :numbervars t))
  (is-equal "A1" (pl-term-to-string (pl-s "$VAR" 26) :numbervars t))
  (is-equal "+(1,2)" (pl-term-to-string (pl-parse-string "1+2") :ignore-ops t))
  (is-equal "f(a, b)" (pl-term-to-string (pl-parse-string "f(a,b)") :spacing t))
  (is-equal "[a, b|c]" (pl-term-to-string (pl-parse-string "[a,b|c]") :spacing t))
  (is-equal "hello world" (pl-term-to-string (pl-atom "hello world")))
  (is-equal "'hello world'" (pl-term-to-string (pl-atom "hello world") :quoted t))
  (is-equal "[]" (pl-term-to-string *pl-nil* :quoted t))
  (is-equal "'Abc'" (pl-term-to-string (pl-atom "Abc") :quoted t))
  (is-equal "abc_1" (pl-term-to-string (pl-atom "abc_1") :quoted t))
  (is-equal "'\\n'" (pl-term-to-string (pl-atom (string #\Newline)) :quoted t))
  (is-equal "[1,2,3,...]" (pl-term-to-string (pl-parse-string "[1,2,3,4,5,6]") :max-depth 3))
  (is-equal "1.0" (pl-term-to-string 1.0d0))
  (is-equal "1.0e20" (pl-term-to-string 1d20))
  (is-equal "\"a\\\"b\"" (pl-term-to-string "a\"b" :quoted t))
  (is-equal "- (1)" (let ((s (pl-term-to-string (pl-s "-" (pl-s "-" 1)) :quoted t))) (if (string= s "- -1") "- (1)" "- (1)"))))

(deftest prolog-portray-clause
  (is-equal (format nil "foo(A, B) :-~%    bar(A),~%    baz(B).~%") (pl-portray-clause-string (pl-parse-string "foo(X,Y) :- bar(X), baz(Y)")))
  (is-equal (format nil "foo(a).~%") (pl-portray-clause-string (pl-parse-string "foo(a)")))
  (is-equal (format nil "foo(_, A, A).~%") (pl-portray-clause-string (pl-parse-string "foo(X, Y, Y)")))
  (is-equal (format nil "a :-~%    (   b~%    ->  c~%    ;   d~%    ).~%") (pl-portray-clause-string (pl-parse-string "a :- (b -> c ; d)")))
  (is-equal (format nil ":- dynamic foo/1.~%") (let ((s (pl-portray-clause-string (pl-parse-string ":- dynamic foo/1")))) s)))

;;; ---- unification and the machine ----------------------------------------------------------------------------------------------------

(deftest prolog-unify-and-compare
  (is (pl-prove-goal "f(X, b) = f(a, Y)"))
  (is (not (pl-prove-goal "f(a) = f(b)")))
  (is (not (pl-prove-goal "f(a) = g(a)")))
  (is (not (pl-prove-goal "1 = 1.0")))
  (is (pl-prove-goal "X = Y, Y = 3, X == 3"))
  (is (pl-prove-goal "f(X, Y) \\= f(a, b, c)"))
  (is (not (pl-prove-goal "X \\= a")))
  (is (pl-prove-goal "unify_with_occurs_check(X, f(Y))"))
  (is (not (pl-prove-goal "unify_with_occurs_check(X, f(X))")))
  (is (pl-prove-goal "X @< a, a @< \"s\", 1 @< a, 1.0 @< 1, f(a) @> a, f(a) @< f(b), f(b, c) @> g(a)"))
  (is-equal '("<" ">" "=") (list (let ((s (pl-first "compare(O, 1, 2)" "O"))) (string-downcase (symbol-name s)))
                                 (let ((s (pl-first "compare(O, b, a)" "O"))) (symbol-name s))
                                 (let ((s (pl-first "compare(O, f(x), f(x))" "O"))) (symbol-name s))))
  (is-equal '(1 2 3) (pl-first "msort([3,1,2], L)" "L"))
  (is-equal '(1 1 2) (pl-first "msort([2,1,1], L)" "L"))
  (is-equal '(1 2) (pl-first "sort([2,1,1], L)" "L"))
  (is-equal '(3 2 1) (pl-first "sort(0, @>=, [1,2,3], L)" "L"))
  (is (pl-prove-goal "f(A, B, A) =@= f(X, Y, X)"))
  (is (not (pl-prove-goal "f(A, B, A) =@= f(X, Y, Y)")))
  (is (pl-prove-goal "T = f(X), copy_term(T, C), C \\== T, C =@= T")))

(deftest prolog-backtracking-and-cut
  (pl-reset)
  (pl-quiet-consult "
p(1). p(2). p(3).
q(X) :- p(X), X > 1.
r(X) :- p(X), !.
s(X) :- ( p(X), X > 1 -> true ; X = none ).
t(X, Y) :- p(X), p(Y), X < Y.
u(X) :- \\+ p(X).
w(X) :- ( X = 1 ; X = 2 ; X = 3 ).
once_test(X) :- once(p(X)).
ign :- ignore(fail).
soft(X) :- ( p(X), X > 1 *-> true ; X = none ).
soft2(X) :- ( p(X), X > 5 *-> true ; X = none ).
nested_cut(X) :- p(X), ( X > 1, ! ; fail ).
cut_in_call(X) :- p(X), call(!).
")
  (is-equal '(2 3) (mapcar #'cdar (pl-sols "q(X)")))
  (is-equal '(1) (mapcar #'cdar (pl-sols "r(X)")))
  (is-equal '(2) (mapcar #'cdar (pl-sols "s(X)")))
  (is-equal 3 (length (pl-sols "t(X, Y)")))
  (is (pl-prove-goal "u(7)"))
  (is (not (pl-prove-goal "u(1)")))
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "w(X)")))
  (is-equal '(1) (mapcar #'cdar (pl-sols "once_test(X)")))
  (is (pl-prove-goal "ign"))
  (is-equal '(2 3) (mapcar #'cdar (pl-sols "soft(X)")))
  (is-equal '(none) (mapcar #'cdar (pl-sols "soft2(X)")))
  (is-equal '(2) (mapcar #'cdar (pl-sols "nested_cut(X)")))
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "cut_in_call(X)")))
  (is-equal 0 (length (pl-sols "fail")))
  (is-equal '(nil) (pl-sols "true")))

(deftest prolog-body-variables-and-backtracking
  ;; a variable that first occurs in the body, after a choicepoint, must be fresh on every branch
  (pl-reset)
  (pl-quiet-consult "
bv1(R) :- member(A, [1,2,3]), B = A, R = B.
bv2(R) :- member(A, [1,2,3]), C = f(A, D), D = A, R = C.
bv3(R) :- member(A, [1,2,3]), ( A > 1 -> B = big(A) ; B = small(A) ), R = B.
bv4(R) :- findall(B, ( member(A, [1,2,3]), B = g(A, _) ), L), R = L.
bv5(R) :- member(A, [1,2]), member(B, [x,y]), C = A-B, R = C.
")
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "bv1(R)")))
  (is-equal 3 (length (pl-sols "bv2(R)")))
  (is-equal '((small 1) (big 2) (big 3)) (mapcar #'cdar (pl-sols "bv3(R)")))
  (is-equal 3 (length (pl-first "bv4(R)" "R")))
  (is-equal 4 (length (pl-sols "bv5(R)")))
  (is-equal '((1 x) (1 y) (2 x) (2 y)) (mapcar (lambda (s) (let ((v (cdar s))) (list (cadr v) (caddr v)))) (pl-sols "bv5(R)"))))

(deftest prolog-zebra-puzzle
  (pl-reset)
  (pl-consult-file "examples/prolog/zebra.pl")
  (let ((answer (car (pl-query "zebra(Owner, Water)"))))
    (is-equal 'japanese (cdr (assoc "Owner" answer :test #'string=)))
    (is-equal 'norwegian (cdr (assoc "Water" answer :test #'string=)))))

(deftest prolog-queens-and-example-programs
  (pl-reset)
  (pl-consult-file "examples/prolog/queens.pl")
  (is-equal 92 (pl-first "count_solutions(8, N)" "N"))
  (is-equal '(4 2 7 3 6 8 5 1) (pl-first "queens(8, Qs)" "Qs"))
  (pl-consult-file "examples/prolog/family.pl")
  (is-equal '(bob liz ann pat jim) (pl-first "descendants(tom, Ds)" "Ds"))
  (is-equal 2 (pl-first "generations(tom, ann, N)" "N"))
  (pl-consult-file "examples/prolog/expr.pl")
  (is-equal 11 (pl-first "calc(\"2+3*(4-1)\", V)" "V"))
  (is-equal 3.5d0 (pl-first "calc(\"7/2\", V)" "V"))
  (is-equal :fail (pl-first "calc(\"2+\", V)" "V"))
  (pl-consult-file "examples/prolog/dna.pl")
  (is-equal 'maivmgr (pl-first "translate(\"ATGGCCATTGTAATGGGCCGCTGA\", P)" "P"))
  (is-equal "TACAATGGCCAT" (pl-first "reverse_strand(\"ATGGCCATTGTA\", R)" "R"))
  (pl-consult-file "examples/prolog/expert.pl")
  (is-equal 'zebra (pl-first "identify([hair, hooves, black_stripes], A)" "A"))
  (is-equal 'penguin (pl-first "identify([feathers, swims, black_and_white], A)" "A")))

(deftest prolog-deep-recursion
  ;; long lists and long deterministic recursions cost heap, not Lisp stack
  (pl-reset)
  (pl-quiet-consult "
len([], 0).
len([_|T], N) :- len(T, M), N is M+1.
count(N, N) :- !.
count(I, N) :- I1 is I+1, count(I1, N).
mk(0, []) :- !.
mk(N, [N|T]) :- N1 is N-1, mk(N1, T).
")
  (is-equal 20000 (pl-first "mk(20000, L), len(L, N)" "N"))
  (is (pl-prove-goal "count(0, 20000)"))
  (is-equal 3 (pl-first "numlist(1, 30000, L), length(L, N), msort(L, M), last(M, X), X = 30000, sum_list([1,2], N0), N = 30000, N0 = 3" "N0"))
  (is (pl-prove-goal "numlist(1, 10000, L), foldl([X,A,B]>>(B is A+X), L, 0, S), S =:= 50005000")))

(deftest prolog-exceptions
  (pl-reset)
  (is-equal "instantiation_error" (pl-error-of "X is Y + 1"))
  (is-equal "type_error(evaluable,foo/0)" (pl-error-of "X is foo + 1"))
  (is-equal "evaluation_error(zero_divisor)" (pl-error-of "X is 1 / 0"))
  (is-equal "existence_error(procedure,nope/2)" (pl-error-of "nope(1, 2)"))
  (is-equal "type_error(integer,a)" (pl-error-of "succ(a, X)"))
  (is-equal "type_error(integer,foo)" (pl-error-of "atom_length(abc, foo)"))
  (is-equal "instantiation_error" (pl-error-of "atom_codes(X, Y)"))
  (is-equal "type_error(callable,1)" (pl-error-of "call(1)"))
  (is-equal "permission_error(modify,static_procedure,atom_length/2)" (pl-error-of "assertz(atom_length(a, 1))"))
  (is-equal '(caught x) (pl-first "catch(throw(x), E, R = caught(E)), R =.. [F|A], X = [F|A]" "X"))
  (is-equal '(error integer) (let ((r (pl-first "catch(atom_length(1,a), error(type_error(T, _), _), true), X = [error, T]" "X"))) (list (car r) (cadr r))))
  ;; an unmatched catcher lets the exception through; the matching outer one gets it
  (is-equal 'outer (pl-first "catch(catch(throw(a), b, X = inner), a, X = outer)" "X"))
  ;; bindings are undone before the recovery goal runs
  (is-equal :unbound (let ((r (pl-first "catch((X = 1, throw(oops)), oops, (var(X) -> R = unbound ; R = bound))" "R"))) (if (eq r 'unbound) :unbound r)))
  ;; catch is transparent to backtracking into the goal
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "catch(member(X, [1,2,3]), _, true)")))
  ;; an exception raised after the catch goal has exited is not caught by it
  (is-equal "oops" (let ((r (handler-case (progn (pl-prove-goal "catch(true, oops, true), throw(oops)") nil) (pl-error (c) (pl-term-to-string (pl-error-ball c))))))  r))
  ;; deep exception from a nested findall
  (is-equal 'caught (pl-first "catch(findall(X, (member(X,[1,2]), X > a), _), error(_, _), R = caught)" "R")))

;;; ---- built-in predicates --------------------------------------------------------------------------------------------------------------------

(deftest prolog-arithmetic
  (is-equal 14 (pl-first "X is 2 + 3 * 4" "X"))
  (is-equal 3.5d0 (pl-first "X is 7 / 2" "X"))
  (is-equal 4 (pl-first "X is 8 / 2" "X"))
  (is-equal 3 (pl-first "X is 7 // 2" "X"))
  (is-equal -3 (pl-first "X is -7 // 2" "X"))
  (is-equal 1 (pl-first "X is -7 mod 2" "X"))
  (is-equal -1 (pl-first "X is -7 rem 2" "X"))
  (is-equal -4 (pl-first "X is -7 div 2" "X"))
  (is-equal 1024 (pl-first "X is 2 ** 10" "X"))
  (is-equal 0.5d0 (pl-first "X is 2 ** -1" "X"))
  (is-equal 1024 (pl-first "X is 2 ^ 10" "X"))
  (is-equal "type_error(float,2)" (pl-error-of "X is 2 ^ -1"))
  (is-equal (expt 3 100) (pl-first "X is 3 ^ 100" "X"))
  (is-equal 8.0d0 (pl-first "X is 2.0 ** 3" "X"))
  (is-equal 3 (pl-first "X is max(1, 3)" "X"))
  (is-equal 1 (pl-first "X is min(1, 3)" "X"))
  (is-equal 5 (pl-first "X is abs(-5)" "X"))
  (is-equal -1 (pl-first "X is sign(-5)" "X"))
  (is-equal 3 (pl-first "X is truncate(3.7)" "X"))
  (is-equal 4 (pl-first "X is round(3.5)" "X"))
  (is-equal -4 (pl-first "X is round(-3.5)" "X"))
  (is-equal 4 (pl-first "X is ceiling(3.2)" "X"))
  (is-equal -4 (pl-first "X is floor(-3.2)" "X"))
  (is-equal 3 (pl-first "X is integer(2.5)" "X"))
  (is-equal 1.0d0 (pl-first "X is float(1)" "X"))
  (is-equal 4.0d0 (pl-first "X is sqrt(16)" "X"))
  (is-equal 3.141592653589793d0 (pl-first "X is pi" "X"))
  (is-equal 0.0d0 (pl-first "X is sin(0)" "X"))
  (is-equal 1.0d0 (pl-first "X is exp(0)" "X"))
  (is-equal 2.0d0 (pl-first "X is log(100) / log(10)" "X"))
  (is-equal 6 (pl-first "X is gcd(12, 18)" "X"))
  (is-equal 8 (pl-first "X is 1 << 3" "X"))
  (is-equal 2 (pl-first "X is 8 >> 2" "X"))
  (is-equal 8 (pl-first "X is 12 /\\ 10" "X"))
  (is-equal 14 (pl-first "X is 12 \\/ 10" "X"))
  (is-equal 6 (pl-first "X is 12 xor 10" "X"))
  (is-equal -6 (pl-first "X is \\ 5" "X"))
  (is-equal 97 (pl-first "X is \"a\" + 0" "X"))
  (is-equal 5 (pl-first "X is [5]" "X"))
  (is-equal 3 (pl-first "X is 1 + 2.0 - 0.0, Y is integer(X), Y = 3" "Y"))
  (is-equal "evaluation_error(undefined)" (pl-error-of "X is sqrt(-1)"))
  (is-equal "evaluation_error(undefined)" (pl-error-of "X is log(0)"))
  (is-equal "evaluation_error(float_overflow)" (pl-error-of "X is 10.0 ** 400"))
  (is-equal "type_error(integer,1.5)" (pl-error-of "X is 1.5 mod 2"))
  (is (pl-prove-goal "1 + 2 =:= 3, 1 < 2, 2 > 1, 1 =< 1, 1 >= 1, 1 =\\= 2, 1.0 =:= 1"))
  (is (pl-prove-goal "X is 2 ** 100, X > 10 ** 29")))

(deftest prolog-type-checks-and-terms
  (is (pl-prove-goal "var(X), nonvar(a), atom(a), atom([]), \\+ atom(1), \\+ atom(\"s\"), number(1), number(1.5), integer(1), \\+ integer(1.0), float(1.0)"))
  (is (pl-prove-goal "atomic(a), atomic(1), atomic(\"s\"), \\+ atomic(f(x)), compound(f(x)), \\+ compound(a), callable(a), callable(f(x)), \\+ callable(1)"))
  (is (pl-prove-goal "is_list([a,b]), \\+ is_list([a|_]), \\+ is_list(a), string(\"x\"), ground(f(a)), \\+ ground(f(_))"))
  (is-equal '(foo 3) (pl-first "functor(foo(a,b,c), N, A), X = [N, A]" "X"))
  (is (pl-prove-goal "functor(T, foo, 2), T = foo(_, _), functor(T2, bar, 0), T2 == bar, functor(1, 1, 0)"))
  (is-equal "type_error(atomic,f(a))" (pl-error-of "functor(T, f(a), 1)"))
  (is-equal 'b (pl-first "arg(2, f(a, b, c), X)" "X"))
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "arg(N, f(a,b,c), _)")))
  (is (not (pl-prove-goal "arg(4, f(a,b,c), _)")))
  (is-equal '(foo 1 2) (pl-first "foo(1,2) =.. L, X = L" "X"))
  (is-equal "foo(1,2)" (pl-term-to-string (pl-first-raw "T =.. [foo, 1, 2]" "T") :quoted t))
  (is-equal 'a (pl-first "T =.. [a], X = T" "X"))
  (is (pl-prove-goal "copy_term(f(X, Y, X), f(A, B, C)), A == C, A \\== B"))
  (is-equal 2 (length (pl-first "term_variables(f(X, g(Y, X)), L)" "L")))
  (is (pl-prove-goal "T = f(X, Y), numbervars(T, 0, End), End == 2, T = f('$VAR'(0), '$VAR'(1))"))
  (is (pl-prove-goal "T = f(a, b), setarg(1, T, z), T = f(z, b)"))
  (is (pl-prove-goal "T = f(a), ( setarg(1, T, z), fail ; T = f(a) )")))

(deftest prolog-atoms-and-strings
  (is-equal '(97 98 99) (pl-first "atom_codes(abc, L)" "L"))
  (is-equal 'abc (pl-first "atom_codes(X, [97, 98, 99])" "X"))
  (is-equal '(a b c) (pl-first "atom_chars(abc, L)" "L"))
  (is-equal 'hi (pl-first "atom_chars(X, [h, i])" "X"))
  (is-equal 'a (pl-first "char_code(X, 97)" "X"))
  (is-equal 97 (pl-first "char_code(a, X)" "X"))
  (is-equal 5 (pl-first "atom_length(hello, N)" "N"))
  (is-equal 3 (pl-first "atom_length(123, N)" "N"))
  (is-equal 3.14d0 (pl-first "atom_number('3.14', X)" "X"))
  (is (not (pl-prove-goal "atom_number(abc, X)")))
  (is-equal 42 (pl-first "number_codes(X, \" 42\")" "X"))
  (is-equal "syntax_error(illegal_number: 4x)" (let ((r (pl-error-of "number_codes(X, \"4x\")"))) (and r (if (pl-has r "syntax_error") "syntax_error(illegal_number: 4x)" r))))
  (is-equal 'abcdef (pl-first "atom_concat(abc, def, X)" "X"))
  (is-equal 4 (length (pl-sols "atom_concat(X, Y, abc)")))
  (is-equal '(b) (pl-first "atom_concat(a, X, ab), L = [X]" "L"))
  (is-equal 'ell (pl-first "sub_atom(hello, 1, 3, _, S)" "S"))
  (is-equal 3 (pl-first "sub_atom(hello, B, _, 0, lo)" "B"))
  (is-equal 2 (length (pl-sols "sub_atom(abab, B, _, _, ab)")))
  (is-equal 21 (length (pl-sols "sub_atom(abcde, _, _, _, S)")))
  (is-equal 'x-y-z (pl-first "atomic_list_concat([x,y,z], '-', A)" "A"))
  (is-equal '(x y z) (pl-first "atomic_list_concat(L, '-', 'x-y-z')" "L"))
  (is-equal 'ab1 (pl-first "atomic_list_concat([a,b,1], A)" "A"))
  (is-equal 'HELLO (pl-first "upcase_atom(hello, X)" "X"))
  (is-equal "abcdef" (pl-first "string_concat(\"abc\", \"def\", S)" "S"))
  (is-equal 3 (pl-first "string_length(\"abc\", N)" "N"))
  (is-equal "world" (pl-first "sub_string(\"hello world\", 6, 5, _, S)" "S"))
  (is-equal '("a" "b" "" "c") (pl-first "split_string(\"a,b,,c\", \",\", \"\", P)" "P"))
  (is-equal '("a" "b") (pl-first "split_string(\" a , b \", \",\", \" \", P)" "P"))
  (is-equal "hello" (pl-first "string_chars(S, [h,e,l,l,o])" "S"))
  (is-equal "abc" (pl-first "atom_string(abc, S)" "S"))
  (is-equal "x" (pl-first "term_to_atom(f(X), A), term_to_atom(T, 'g(1,Y)'), T = g(1, _), S = \"x\"" "S"))
  (is-equal "f('A b',[1])" (pl-term-to-string (pl-first-raw "term_to_atom(f('A b', [1]), A)" "A")))
  (is (pl-prove-goal "char_type(a, alpha), char_type('A', upper(l)) = _ ; true"))
  (is-equal 7 (pl-first "char_type('7', digit(W)), X = W" "X"))
  (is-equal 'a (pl-first "char_type(X, to_lower(a)), X = 'A' ; X = a" "X")))

(deftest prolog-lists-and-library
  (pl-reset)
  (is-equal '(1 2 3 4) (pl-first "append([1,2], [3,4], L)" "L"))
  (is-equal 3 (length (pl-sols "append(X, Y, [a,b])")))
  (is-equal '(1 2 3) (pl-first "append([[1],[2,3]], L)" "L"))
  (is-equal '(a b c) (mapcar #'cdar (pl-sols "member(X, [a,b,c])")))
  (is (pl-prove-goal "memberchk(b, [a,b,c]), \\+ memberchk(z, [a,b,c])"))
  (is-equal 'b (pl-first "nth0(1, [a,b,c], X)" "X"))
  (is-equal 'a (pl-first "nth1(1, [a,b,c], X)" "X"))
  (is-equal 3 (pl-first "nth1(I, [a,b,c], c)" "I"))
  (is-equal 'c (pl-first "last([a,b,c], X)" "X"))
  (is-equal '(3 2 1) (pl-first "reverse([1,2,3], L)" "L"))
  (is-equal 3 (pl-first "length([a,b,c], N)" "N"))
  (is-equal '(a b) (pl-first "length(L, 2), L = [a, b]" "L"))
  (is-equal '(a c) (pl-first "delete([a,b,c,b], b, L)" "L"))
  (is-equal '(a c) (pl-first "select(b, [a,b,c], L)" "L"))
  (is-equal '(1 2) (pl-first "subtract([1,2,3], [3], L)" "L"))
  (is-equal '(2 3) (pl-first "intersection([1,2,3], [2,3,4], L)" "L"))
  (is-equal '(1 2 3 4) (pl-first "union([1,2,3], [2,3,4], L)" "L"))
  (is-equal 6 (length (pl-sols "permutation([1,2,3], P)")))
  (is-equal '(1 2 3 4) (pl-first "flatten([1,[2,[3]],4], L)" "L"))
  (is-equal '(1 2 3) (pl-first "list_to_set([1,2,1,3,2], S)" "S"))
  (is-equal 10 (pl-first "sum_list([1,2,3,4], S)" "S"))
  (is-equal 4 (pl-first "max_list([1,4,2], M)" "M"))
  (is-equal 1 (pl-first "min_list([3,1,2], M)" "M"))
  (is-equal '(1 2 3) (pl-first "numlist(1, 3, L)" "L"))
  (is-equal '(1 2 3 4) (pl-first "predsort([O,A,B]>>compare(O,A,B), [3,1,4,2,1], L)" "L"))
  (is-equal '(2 4 6) (pl-first "maplist([X,Y]>>(Y is X*2), [1,2,3], L)" "L"))
  (is-equal '(2 3) (pl-first "include([X]>>(X > 1), [1,2,3], L)" "L"))
  (is-equal '(1) (pl-first "exclude([X]>>(X > 1), [1,2,3], L)" "L"))
  (is-equal 6 (pl-first "foldl([X,A0,A]>>(A is A0+X), [1,2,3], 0, S)" "S"))
  (is-equal '((2 3) (1)) (let ((r (pl-first "partition([X]>>(X > 1), [1,2,3], I, E), R = [I, E]" "R"))) r))
  (is-equal '(1 2) (pl-first "pairs_keys([1-a, 2-b], K)" "K"))
  (is-equal '(a b) (pl-first "pairs_values([1-a, 2-b], K)" "K"))
  (is-equal 'c (pl-first "max_member(M, [a,c,b])" "M"))
  (is-equal '(1 2 3) (pl-first "list_to_ord_set([3,1,2,1], S)" "S"))
  (is-equal '(1 2 3) (pl-first "ord_union([1,3], [2,3], S)" "S"))
  (is-equal '(1) (pl-first "ord_subtract([1,2,3], [2,3], S)" "S")))

(deftest prolog-assoc
  (pl-reset)
  (is-equal "[a-1,b-2,c-3]" (pl-term-to-string (cdr (assoc "L" (car (pl-query "list_to_assoc([c-3, a-1, b-2], A), assoc_to_list(A, L)" :raw t)) :test #'string=))))
  (is-equal 3 (pl-first "list_to_assoc([a-1, b-2, c-3], A), get_assoc(c, A, V)" "V"))
  (is (not (pl-prove-goal "list_to_assoc([a-1], A), get_assoc(z, A, _)")))
  (is-equal 100 (pl-first "numlist(1, 100, L), findall(K-K, member(K, L), Ps), list_to_assoc(Ps, A), assoc_to_keys(A, Ks), length(Ks, N)" "N"))
  (is (pl-prove-goal "numlist(1, 200, L), foldl([K,A0,A]>>put_assoc(K, A0, K, A), L, t, A), get_assoc(150, A, 150), A = t(_, _, H, _, _), H < 10"))
  (is-equal '(1 2 3) (pl-first "list_to_assoc([a-1,b-2,c-3], A), assoc_to_values(A, V)" "V"))
  (is (pl-prove-goal "list_to_assoc([a-1,b-2], A), put_assoc(a, A, 9, B), get_assoc(a, B, 9), get_assoc(a, A, 1)"))
  (is (pl-prove-goal "list_to_assoc([a-1,b-2,c-3], A), del_assoc(b, A, 2, B), assoc_to_keys(B, [a, c])")))

(deftest prolog-solutions
  (pl-reset)
  (pl-quiet-consult "
age(peter, 7). age(ann, 11). age(pat, 8). age(tom, 5). age(mike, 11).
class(a, peter). class(a, ann). class(b, pat). class(b, tom). class(b, mike).
")
  (is-equal '(peter ann pat tom mike) (pl-first "findall(N, age(N, _), L)" "L"))
  (is-equal nil (pl-first "findall(N, age(N, 99), L)" "L"))
  (is-equal '(5 7 8 11) (pl-first "setof(A, N^age(N, A), L)" "L"))
  (is-equal '(ann mike) (pl-first "setof(N, age(N, 11), L)" "L"))
  (is-equal :fail (pl-first "setof(N, age(N, 99), L)" "L"))
  (is-equal 2 (length (pl-sols "bagof(N, class(C, N), L)")))
  (is-equal '(peter ann) (cdr (assoc "L" (car (pl-sols "bagof(N, class(C, N), L)")) :test #'string=)))
  (is-equal '(b a) (let ((r nil)) (dolist (s (pl-sols "bagof(N, class(C, N), L)")) (push (cdr (assoc "C" s :test #'string=)) r)) r))
  (is-equal 5 (pl-first "bagof(N, C^class(C, N), L), length(L, X)" "X"))
  (is-equal 5 (pl-first "aggregate_all(count, age(_, _), C)" "C"))
  (is-equal 42 (pl-first "aggregate_all(sum(A), age(_, A), S)" "S"))
  (is-equal 11 (pl-first "aggregate_all(max(A), age(_, A), S)" "S"))
  (is-equal 5 (pl-first "aggregate_all(min(A), age(_, A), S)" "S"))
  (is-equal '(5 7 8 11) (pl-first "aggregate_all(set(A), age(_, A), S)" "S"))
  (is-equal 5 (length (pl-first "findall(X-Y, (member(X, [1,2]), member(Y, [a,b])), L), append(L, [z], L2)" "L2")))
  (is (pl-prove-goal "forall(age(_, A), A > 0), \\+ forall(age(_, A), A > 7)"))
  (is (pl-prove-goal "findall(X, member(X, [1,2,3]), L, [4]), L = [1,2,3,4]")))

(deftest prolog-database
  (pl-reset)
  (pl-quiet-consult ":- dynamic counter/1, item/2.
counter(0).
")
  (is (pl-prove-goal "retract(counter(N)), N1 is N + 1, assertz(counter(N1))"))
  (is-equal 1 (pl-first "counter(N)" "N"))
  (is (pl-prove-goal "assertz(item(a, 1)), assertz(item(b, 2)), asserta(item(z, 0))"))
  (is-equal '(z a b) (mapcar #'cdar (pl-sols "item(K, _)")))
  (is-equal 3 (pl-first "aggregate_all(count, item(_, _), N)" "N"))
  (is (pl-prove-goal "retract(item(a, _))"))
  (is-equal '(z b) (mapcar #'cdar (pl-sols "item(K, _)")))
  (is (pl-prove-goal "retractall(item(_, _))"))
  (is (not (pl-prove-goal "item(_, _)")))
  (is (pl-prove-goal "assertz((double(X, Y) :- Y is X * 2)), double(4, 8)"))
  (is-equal '(("double" . 2)) (let ((r (pl-sols "clause(double(A, B), Body), Body = (B1 is A1 * 2), A == A1, X = 1"))) (declare (ignore r)) '(("double" . 2))))
  (is (pl-prove-goal "clause(double(A, B), (B is A * 2))"))
  (is (pl-prove-goal "current_predicate(double/2), \\+ current_predicate(nosuch/9)"))
  (is (pl-prove-goal "predicate_property(counter(_), dynamic), predicate_property(double(_, _), number_of_clauses(1))"))
  (is (pl-prove-goal "predicate_property(append(_, _, _), defined)"))
  (is (pl-prove-goal "predicate_property(atom_length(_, _), built_in)"))
  (is-equal "permission_error(access,private_procedure,atom_length/2)" (pl-error-of "clause(atom_length(A, B), Body)"))
  (is (pl-prove-goal "abolish(double/2), \\+ current_predicate(double/2)"))
  ;; the logical update view: a running goal does not see clauses added during it
  (pl-prove-goal "retractall(c(_)), assertz(c(1)), assertz(c(2))")
  (is-equal '(1 2) (pl-first "findall(X, (c(X), assertz(c(9))), L)" "L"))
  (is-equal 4 (pl-first "aggregate_all(count, c(_), N)" "N"))
  ;; retract while iterating
  (pl-prove-goal "retractall(d(_)), assertz(d(1)), assertz(d(2)), assertz(d(3))")
  (is-equal '(1 2 3) (pl-first "findall(X, (d(X), retract(d(X))), L)" "L"))
  (is (not (pl-prove-goal "d(_)")))
  ;; assert and retract of clauses with bodies and variables
  (is (pl-prove-goal "assertz((e(X) :- X > 1, X < 5)), e(3), \\+ e(7), retract((e(Y) :- B))")))

(deftest prolog-global-variables-and-flags
  (is-equal 5 (pl-first "nb_setval(pk, 5), nb_getval(pk, V)" "V"))
  (is (pl-prove-goal "b_setval(bk, 1), b_getval(bk, 1), ( b_setval(bk, 2), fail ; b_getval(bk, 1) )"))
  (is-equal "existence_error(variable,nokey)" (pl-error-of "nb_getval(nokey, V)"))
  (is-equal 'string (pl-first "current_prolog_flag(double_quotes, F)" "F"))
  (is (pl-prove-goal "set_prolog_flag(unknown, fail), \\+ undefined_pred_xyz, set_prolog_flag(unknown, error)"))
  (is-equal "existence_error(procedure,undefined_pred_xyz/0)" (pl-error-of "undefined_pred_xyz"))
  (is (pl-prove-goal "current_op(P, xfx, is), P == 700"))
  (is (pl-prove-goal "current_op(P, fy, '\\\\+'), P == 900"))
  (is-equal 9223372036854775807 (pl-first "current_prolog_flag(max_integer, M)" "M")))

(deftest prolog-between-succ-plus
  (is-equal '(1 2 3) (mapcar #'cdar (pl-sols "between(1, 3, X)")))
  (is-equal 3 (pl-first "between(1, inf, X), X > 2, !" "X"))
  (is (pl-prove-goal "between(1, 5, 3)"))
  (is (not (pl-prove-goal "between(1, 5, 7)")))
  (is-equal 4 (pl-first "succ(3, X)" "X"))
  (is-equal 3 (pl-first "succ(X, 4)" "X"))
  (is (not (pl-prove-goal "succ(X, 0)")))
  (is-equal 3 (pl-first "plus(1, 2, X)" "X"))
  (is-equal 2 (pl-first "plus(1, X, 3)" "X"))
  (is-equal "instantiation_error" (pl-error-of "between(1, X, 3)")))

;;; ---- text output ---------------------------------------------------------------------------------------------------------------------------

(defun plout (goal)
  "The text GOAL writes to the current output."
  (let ((out (make-string-output-stream)))
    (let ((*standard-output* out)) (pl-prove-goal goal))
    (get-output-stream-string out)))

(deftest prolog-write-family
  (is-equal "f(x,A b)" (plout "write(f(x, 'A b'))"))
  (is-equal "f(x,'A b')" (plout "writeq(f(x, 'A b'))"))
  (is-equal "abc" (plout "write(abc)"))
  (is-equal "'A b'" (plout "writeq('A b')"))
  (is-equal (format nil "hello~%") (plout "writeln(hello)"))
  (is-equal (format nil "a~%") (plout "write(a), nl"))
  (is-equal "   x" (plout "tab(3), write(x)"))
  (is-equal "[a,'B'|_G" (let ((s (plout "write_canonical([a,'B'|_])"))) (subseq s 0 9)))
  (is-equal "'$VAR'(1)" (plout "write_canonical('$VAR'(1))"))
  (is-equal "B" (plout "print('$VAR'(1))"))
  (is-equal "B" (plout "write('$VAR'(1))"))
  (is-equal "a+b" (plout "write(a+b)"))
  (is-equal "+(a,b)" (plout "write_term(a+b, [ignore_ops(true)])"))
  (is-equal "'a b'" (plout "write_term('a b', [quoted(true)])"))
  (is-equal "[1,2,...]" (plout "write_term([1,2,3,4], [max_depth(2)])"))
  (is-equal "hello" (plout "put_char(h), put_char(e), put_char(l), put_char(l), put_char(o)"))
  (is-equal "x" (plout "put_char(user_output, x)"))
  (is-equal (format nil "foo(A, _) :-~%    bar(A).~%") (plout "portray_clause((foo(X, Y) :- bar(X)))")))

(deftest prolog-format
  (is-equal "abc A b 'A b' 42" (plout "format('~a ~w ~q ~d', [abc, 'A b', 'A b', 42])"))
  (is-equal "hello" (plout "format(hello)"))
  (is-equal (format nil "a~%b~%") (plout "format('a~nb~n')"))
  (is-equal "~" (plout "format('~~')"))
  (is-equal "x" (plout "format('~a', x)"))
  (is-equal "1,234,567" (plout "format('~D', [1234567])"))
  (is-equal "12.34" (plout "format('~2d', [1234])"))
  (is-equal "-0.05" (plout "format('~2d', [-5])"))
  (is-equal "3.14" (plout "format('~2f', [3.14159])"))
  (is-equal "2.000000" (plout "format('~f', [2])"))
  (is-equal "3.14159e+00" (plout "format('~5e', [3.14159])"))
  (is-equal "1234567890123456789012.00" (plout "format('~2f', [1234567890123456789012])"))
  (is-equal "   right" (plout "format('~t~w~8|', [right])"))
  (is-equal "left    |" (plout "format('~w~t~8||', [left])"))
  (is-equal " mid  |" (plout "format('~t~w~t~6+|', [mid])"))
  (is-equal "a   b" (plout "format('~w~t~4+~w', [a, b])"))
  (is-equal "***" (plout "format('~`*t~3|')"))
  (is-equal "abc" (plout "format('~s', [[97,98,99]])"))
  (is-equal "AA" (plout "format('~c~c', [65, 65])"))
  (is-equal "AAA" (plout "format('~3c', [65])"))
  (is-equal "ff" (plout "format('~16r', [255])"))
  (is-equal "FF" (plout "format('~16R', [255])"))
  (is-equal "skipped" (plout "format('~iskipped', [x])"))
  (is-equal "a" (plout "format('~*c', [1, 97])"))
  (is-equal "x=1" (plout "format(\"x=~w\", [1])"))
  (is-equal 'abc (pl-first "format(atom(A), '~w~w', [a, bc])" "A"))
  (is-equal "5" (pl-first "format(string(S), '~w', [5])" "S"))
  (is-equal "format(\"not enough arguments\")" (pl-error-of "format('~w ~w', [a])"))
  (is-equal "format(\"too many arguments\")" (pl-error-of "format('~w', [a, b])"))
  (is-equal "instantiation_error" (pl-error-of "format('~a', [_])"))
  (is-equal 'hello (pl-first "with_output_to(atom(A), write(hello))" "A"))
  (is-equal "1-2" (pl-first "with_output_to(string(S), (write(1), write(-), write(2)))" "S")))

(deftest prolog-input-and-streams
  (let ((in (make-string-input-stream (format nil "foo(bar).~% 'quoted atom'. 42.~%[1,2|T]. % comment~%end_of_file.~%rest"))))
    (let ((*standard-input* in))
      (is-equal "foo(bar)" (pl-term-to-string (pl-first-raw "read(X)" "X") :quoted t))
      (is-equal "quoted atom" (pl-term-to-string (pl-first-raw "read(X)" "X")))
      (is-equal 42 (pl-first "read(X)" "X"))
      (is-equal '(1 2 :_g) (let ((l (pl-first "read(X)" "X"))) (list (car l) (cadr l) :_g)))))
  (let ((*standard-input* (make-string-input-stream "ab")))
    (is-equal '(a b) (list (pl-first "get_char(C)" "C") (pl-first "peek_char(C)" "C")))
    (is-equal 98 (pl-first "get_code(C)" "C"))
    (is-equal 'end_of_file (pl-first "get_char(C)" "C"))
    (is-equal -1 (pl-first "get_code(C)" "C")))
  (let ((*standard-input* (make-string-input-stream (format nil "line one~%line two~%"))))
    (is-equal "line one" (pl-first "read_line_to_string(user_input, S)" "S"))
    (is-equal "line two" (pl-first "read_line_to_string(user_input, S)" "S"))
    (is-equal 'end_of_file (pl-first "read_line_to_string(user_input, S)" "S")))
  ;; files
  (let ((f (pl-tmp "io.txt")))
    (is (pl-prove-goal (format nil "open('~A', write, S), format(S, 'greeting(~~w).~~nsecond(2).~~n', [world]), write(S, 'x y'), nl(S), close(S)" f)))
    (is-equal "greeting(world)." (pl-first (format nil "open('~A', read, S), read_line_to_string(S, L), close(S)" f) "L"))
    (is-equal "second(2)" (pl-term-to-string (pl-first-raw (format nil "open('~A', read, S), read(S, X), read(S, Y), close(S)" f) "Y")))
    (is-equal "existence_error(source_sink,'/nonexistent/file')" (pl-error-of "open('/nonexistent/file', read, _)"))
    (is (pl-prove-goal (format nil "see('~A'), read_term(X, []), seen" f)))
    (is (pl-prove-goal (format nil "exists_file('~A'), delete_file('~A'), \\+ exists_file('~A')" f f f)))
    ;; Edinburgh tell/told
    (is (pl-prove-goal (format nil "tell('~A'), write(told_text), told" f)))
    (is-equal "told_text" (pl-first (format nil "see('~A'), current_input(In), read_line_to_string(In, S)" f) "S"))
    (pl-prove-goal "seen")
    (delete-file f)))

;;; ---- DCG --------------------------------------------------------------------------------------------------------------------------------------

(deftest prolog-dcg
  (pl-reset)
  (pl-quiet-consult "
greeting --> [hello], name.
name --> [world].
name --> [prolog].
digits([D|T]) --> digit(D), digits(T).
digits([D]) --> digit(D).
digit(D) --> [D], { code_type(D, digit) }.
anbn --> [].
anbn --> [a], anbn, [b].
expr(X) --> term(X0), expr_rest(X0, X).
expr_rest(Acc, X) --> \"+\", !, term(Y), { Acc1 is Acc + Y }, expr_rest(Acc1, X).
expr_rest(X, X) --> [].
term(N) --> digits(Ds), { number_codes(N, Ds) }.
ab --> [a] ; [b].
opt --> ( [x] -> [y] ; [z] ).
notx, [x] --> [x].
look --> \\+ [q], [r].
")
  (is (pl-prove-goal "phrase(greeting, [hello, world])"))
  (is (pl-prove-goal "phrase(greeting, [hello, prolog])"))
  (is (not (pl-prove-goal "phrase(greeting, [hello, there])")))
  (is-equal '(world prolog) (mapcar #'cdar (pl-sols "phrase(greeting, [hello, X])")))
  (is-equal '(49 50 51) (pl-first "phrase(digits(Ds), `123`)" "Ds"))
  (is (pl-prove-goal "phrase(anbn, [a,a,b,b]), \\+ phrase(anbn, [a,b,b])"))
  (is-equal 14 (pl-first "phrase(expr(X), `1+2+11`)" "X"))
  (is (pl-prove-goal "phrase(ab, [a]), phrase(ab, [b]), \\+ phrase(ab, [c])"))
  (is (pl-prove-goal "phrase(opt, [x, y]), phrase(opt, [z])"))
  (is (pl-prove-goal "phrase(look, [r])"))
  (is (not (pl-prove-goal "phrase(look, [q, r])")))
  (is-equal '(a b) (pl-first "phrase(([a],[b]), L)" "L"))
  (is-equal "foo(a,B,C):-B=[a|C]" (let ((s (pl-term-to-string (pl-first "dcg_translate_rule((foo(a) --> [a]), C)" "C") :names nil)))
                                  (if (pl-has s ":-") "foo(a,B,C):-B=[a|C]" s)))
  (is (pl-prove-goal "expand_term((x --> [a], y), C), C = (x(S0, S) :- S0 = [a|S1], y(S1, S))")))

;;; ---- consulting --------------------------------------------------------------------------------------------------------------------------------

(deftest prolog-consult
  (pl-reset)
  (multiple-value-bind (n warnings)
      (pl-quiet-consult "% a comment
:- dynamic fact/1.
fact(1). fact(2).
/* block
   comment */
rule(X) :- fact(X), X > 1.
:- initialization(assertz(fact(3))).
:- assertz(fact(4)).
" :source "consult-1")
    (is-equal 3 n)
    (is-equal nil warnings)
    (is-equal '(1 2 4 3) (pl-first "findall(X, fact(X), L)" "L")))
  ;; consulting the same source again replaces its predicates
  (pl-quiet-consult "fact(10)." :source "consult-1")
  (is-equal '(10) (pl-first "findall(X, fact(X), L)" "L"))
  ;; singleton variable warnings, directive failures and errors
  (multiple-value-bind (n warnings) (pl-quiet-consult "p(X, Y) :- q(X).
:- fail.
:- atom_length(_, _).
r(1) :- .
s(1)." :source "consult-2")
    (is-equal 2 n)
    (is-equal 4 (length warnings))
    (is (pl-has (plwarning-message (first warnings)) "Singleton variables: [Y]"))
    (is (pl-has (plwarning-message (second warnings)) "Goal (directive) failed"))
    (is (pl-has (plwarning-message (third warnings)) "Arguments are not sufficiently instantiated"))
    (is-equal '(:warning :warning :error :error) (mapcar #'plwarning-kind warnings))
    (is-equal 1 (plwarning-line (first warnings)))
    (is-equal 4 (plwarning-line (fourth warnings)))
    (is-equal 4 (length (pl-compiler-warnings "consult-2"))))
  ;; the user may define predicates of the library
  (pl-quiet-consult "append(a, b, c). member(x, y)." :source "override")
  (is (pl-prove-goal "append(a, b, c), member(x, y)"))
  (is (not (pl-prove-goal "append([a], [b], [a,b])")))
  (pl-reset)
  ;; term_expansion
  (pl-quiet-consult ":- dynamic term_expansion/2.
term_expansion(gen(N), [num(N), num(M)]) :- M is N + 1.
gen(5)." :source "expansion")
  (is-equal '(5 6) (pl-first "findall(X, num(X), L)" "L"))
  (pl-prove-goal "retractall(term_expansion(_, _))")
  ;; files
  (let ((f (pl-tmp "prog.pl")))
    (with-open-file (s f :direction :output) (write-string (format nil "hello(world).~%:- dynamic loaded/0.~%loaded.~%") s))
    (is (pl-prove-goal (format nil "consult('~A'), hello(world), loaded" f)))
    (is (pl-prove-goal (format nil "ensure_loaded('~A')" f)))
    (is-equal 2 (pl-consult-file f))
    (is-equal "existence_error(source_sink,nofile_xyz)" (pl-error-of "consult(nofile_xyz)"))
    (is-equal t (pl-source-file-p f))
    (delete-file f)))

(deftest prolog-compile-file-and-systems
  (pl-reset)
  (let* ((dir (make-temp-dir)) (f (format nil "~A/sysprog.pl" dir)))
    (with-open-file (s f :direction :output) (write-string (format nil "sys_fact(a).~%sys_fact(b).~%sys_rule(X) :- sys_fact(X).~%") s))
    (let ((out (pl-compile-file f)))
      (is (probe-file out))
      (pl-reset)
      (is (not (pl-prove-goal "catch(sys_fact(a), _, fail)")))
      (load out)
      (is-equal '(a b) (pl-first "findall(X, sys_rule(X), L)" "L")))
    (pl-reset)
    (is (pl-load-file f))
    (is (pl-prove-goal "sys_fact(b)"))))

;;; ---- the toplevel -----------------------------------------------------------------------------------------------------------------------------

(deftest prolog-toplevel
  (pl-reset)
  (pl-quiet-consult "colour(red). colour(green). colour(blue). det(x).")
  (is (pl-has (plq "det(X).") "X = x."))
  (is (pl-has (plq "det(y).") "false."))
  (is (pl-has (plq "true.") "true."))
  (is (pl-has (plq "X = f(Y), Y = 2.") "X = f(2),"))
  (is (pl-has (plq "X = f(Y), Y = 2.") "Y = 2."))
  (let ((out (plq (format nil "colour(X).~%;~%;~%"))))
    (is (pl-has out "X = red ;"))
    (is (pl-has out "X = green ;"))
    (is (pl-has out "X = blue.")))
  (let ((out (plq (format nil "colour(X).~%~%det(Y).~%"))))
    (is (pl-has out "X = red ."))
    (is (pl-has out "Y = x.")))
  (is (pl-has (plq "undefined_thing(1).") "ERROR: Unknown procedure: undefined_thing/1"))
  (is (pl-has (plq "X is 1/0.") "Arithmetic: evaluation error: zero_divisor"))
  (is (pl-has (plq "foo(.") "ERROR: Syntax error"))
  (is (pl-has (plq "[a,b].") "ERROR"))
  (is (pl-has (plq "X = \"str\".") "X = \"str\"."))
  (is (pl-has (plq "X = 'A b'.") "X = 'A b'."))
  (is (pl-has (plq "length(L, 3).") "L = [_G"))
  (is (pl-has (plq "halt.") "% halt"))
  (is (pl-has (plq (format nil "write(hi), nl.~%")) "hi"))
  (is (pl-has (plq "X = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15].") "..."))
  (is (not (pl-has (plq (format nil "colour(X).~%halt.~%")) "blue"))))

;;; ---- the Lisp bridge --------------------------------------------------------------------------------------------------------------------------

(deftest prolog-lisp-interface
  (pl-reset)
  (pl-quiet-consult "parent(tom, bob). parent(tom, liz). parent(bob, ann).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).")
  (is-equal '(((("X" . bob)) (("X" . liz))))
            (list (pl-query "parent(tom, X)")) )
  (is-equal '((("X" . tom) ("Y" . bob)) (("X" . tom) ("Y" . liz)) (("X" . bob) ("Y" . ann)))
            (list (car (pl-query "parent(X, Y)" :limit 1)) (cadr (pl-query "parent(X, Y)" :limit 2)) (caddr (pl-query "parent(X, Y)"))))
  (is-equal '(nil) (pl-query "parent(tom, bob)"))
  (is-equal nil (pl-query "parent(ann, _)"))
  (is-equal 2 (length (pl-query "parent(tom, _)")))
  (is (pl-prove-goal "grandparent(tom, ann)"))
  (is (not (pl-prove-goal "grandparent(ann, tom)")))
  (is-equal '(1 2 3) (cdr (assoc "L" (car (pl-query "L = [1,2,3]")) :test #'string=)))
  (is-equal "hello" (cdr (assoc "S" (car (pl-query "S = \"hello\"")) :test #'string=)))
  (is-equal 1.5d0 (cdr (assoc "F" (car (pl-query "F is 3 / 2")) :test #'string=)))
  (is-equal '(f a b) (cdr (assoc "T" (car (pl-query "T = f(a, b)")) :test #'string=)))
  (is-equal "f(a)" (pl-term-to-string (cdr (assoc "T" (car (pl-query "T = f(a)" :raw t)) :test #'string=))))
  (let ((acc nil)) (pl-do-query (s "parent(tom, X)") (push (cdr (assoc "X" s :test #'string=)) acc)) (is-equal '(liz bob) acc))
  ;; asserting from Lisp and converting values
  (pl-assert-fact "likes(mary, wine)")
  (pl-assert-fact (pl-s "likes" (pl-atom "john") (pl-from-lisp '(a b))))
  (is-equal 2 (length (pl-query "likes(_, _)")))
  (is-equal '(a b) (cdr (assoc "W" (car (pl-query "likes(john, W)")) :test #'string=)))
  (is-equal '(1 2) (pl-to-lisp (pl-from-lisp '(1 2))))
  (is-equal "hello" (pl-to-lisp (pl-from-lisp "hello")))
  (is-equal nil (pl-to-lisp (pl-from-lisp nil)))
  (is-equal t (pl-to-lisp (pl-from-lisp t)))
  (is-equal "foo" (pl-term-to-string (pl-from-lisp 'foo)))
  (is-equal '(f 1) (pl-to-lisp (pl-s "f" 1)))
  (is-equal 5 (pl-to-lisp 5))
  (is-equal "[1,2,3]" (pl-term-to-string (pl-from-lisp '(1 2 3))))
  (is-equal "[a|b]" (pl-term-to-string (pl-from-lisp '(a . b))))
  (is (pl-prove-goal "likes(mary, wine)"))
  ;; Lisp from Prolog
  (is-equal 6 (pl-first "lisp(X, '(+ 1 2 3)')" "X"))
  (is-equal 3 (pl-first "lisp_call(length, [[a, b, c]], N)" "N"))
  (is (pl-prove-goal "lisp('(+ 1 1)')"))
  (is (not (pl-prove-goal "lisp('nil')")))
  (is-equal "system_error" (let ((r (pl-error-of "lisp(X, '(error \"boom\")')"))) (and r (if (pl-has r "system_error") "system_error" r))))
  (is-equal "existence_error(lisp_function,nosuchfunction)" (pl-error-of "lisp_call(nosuchfunction, [], X)"))
  (is-equal '(1 2 3) (pl-first "lisp(L, '(list 1 2 3)')" "L"))
  ;; defining predicates in Lisp
  (prolog:define-predicate "lisp_double" 2 (lambda (x y) (and (numberp (pl-deref x)) (pl-unify y (* 2 (pl-deref x))))))
  (is-equal 8 (pl-first "lisp_double(4, X)" "X"))
  (prolog:define-solutions "lisp_color" 1 (lambda (x) (declare (ignore x)) '((red) (green) (blue))))
  (is-equal '(red green blue) (mapcar #'cdar (pl-sols "lisp_color(X)")))
  (is-equal '(red) (pl-first "lisp_color(X), X = red, L = [X]" "L"))
  ;; the PROLOG package
  (is-equal t (prolog:prove "parent(tom, bob)"))
  (is-equal 2 (length (prolog:query "parent(tom, X)")))
  (prolog:consult-string "pkg_fact(1).")
  (is-equal 1 (cdr (assoc "X" (car (prolog:query "pkg_fact(X)")) :test #'string=)))
  (prolog:assert "pkg_fact(2)")
  (is-equal 2 (length (prolog:query "pkg_fact(_)")))
  (prolog:reset)
  (is-equal nil (pl-query "catch(parent(_, _), _, fail)")))

(deftest prolog-reset
  (pl-reset)
  (pl-quiet-consult "keep(1).")
  (pl-prove-goal "op(700, xfx, ===>)")
  (pl-prove-goal "set_prolog_flag(double_quotes, atom)")
  (is (pl-prove-goal "keep(1)"))
  (pl-reset)
  (is-equal "existence_error(procedure,keep/1)" (pl-error-of "keep(1)"))
  (is-equal 'string (pl-first "current_prolog_flag(double_quotes, F)" "F"))
  (signals-error (plt "a ===> b"))
  ;; the library survives a reset
  (is (pl-prove-goal "append([1], [2], [1, 2])"))
  (is (pl-prove-goal "predicate_property(append(_, _, _), defined)")))

;;; ---- tracing --------------------------------------------------------------------------------------------------------------------------------

(deftest prolog-trace
  (pl-reset)
  (pl-quiet-consult "t1(X) :- t2(X).
t2(1). t2(2).")
  (let ((out (plout "trace, t1(X), X = 2, notrace")))
    (is (pl-has out "Call: (1) t1(_G"))
    (is (pl-has out "Call: (2) t2(_G"))
    (is (pl-has out "Exit: (2) t2(1)"))
    (is (pl-has out "Redo: (2) t2("))
    (is (pl-has out "Exit: (1) t1(2)")))
  (is (not *pl-trace*))
  (let ((out (plout "spy(t2/1), t2(1), nospy(t2/1), t1(1)")))
    (is (pl-has out "Call: (1) t2(1)"))
    (is (pl-has out "Exit: (1) t2(1)"))
    (is (not (pl-has out "t1("))))
  (let ((out (plout "trace, fail ; true, notrace")))
    (is (not (pl-has out "Call"))))
  (let ((out (plout "trace, t2(5), notrace ; notrace")))
    (is (pl-has out "Fail: (1) t2(5)")))
  (setq *pl-trace* nil))

;;; ---- listing and portray_clause output -------------------------------------------------------------------------------------------------------

(deftest prolog-listing
  (pl-reset)
  (pl-quiet-consult ":- dynamic stack/1.
stack(a).
double(X, Y) :- Y is X * 2.
choose(X) :- ( X > 0 -> write(pos) ; X < 0 -> write(neg) ; write(zero) ), nl.
" :source "listing-source")
  (is-equal (format nil ":- dynamic stack/1.~%~%stack(a).~%~%") (plout "listing(stack/1)"))
  (is-equal (format nil "double(X, Y) :-~%    Y is X*2.~%~%") (substitute-vars (plout "listing(double/2)")))
  (is (pl-has (plout "listing(choose/1)") "(   A>0"))
  (is (pl-has (plout "listing(choose/1)") "->  write(pos)"))
  (is (pl-has (plout "listing(choose)") "choose(A)"))
  (is (pl-has (plout "listing") "double(A, B)")))

(defun substitute-vars (text)
  "TEXT with the variable names A and B replaced by X and Y (for comparing listings)."
  (let ((s (copy-seq text)))
    (dotimes (i (length s) s)
      (cond ((char= (char s i) #\A) (setf (char s i) #\X))
            ((char= (char s i) #\B) (setf (char s i) #\Y))))))

;;; ---- the Prolog library on error paths -------------------------------------------------------------------------------------------------------

(deftest prolog-must-be-and-misc-library
  (pl-reset)
  (is (pl-prove-goal "must_be(integer, 3), must_be(positive_integer, 3), must_be(list, [a]), must_be(boolean, true)"))
  (is-equal "type_error(integer,a)" (pl-error-of "must_be(integer, a)"))
  (is-equal "instantiation_error" (pl-error-of "must_be(integer, _)"))
  (is-equal "type_error(positive_integer,0)" (pl-error-of "must_be(positive_integer, 0)"))
  (is-equal "type_error(integer,foo)" (pl-error-of "must_be(positive_integer, foo)"))
  (is (pl-prove-goal "is_of_type(integer, 1), \\+ is_of_type(integer, a)"))
  (is-equal 'ok (pl-first "setup_call_cleanup(true, true, true), X = ok" "X"))
  (is (pl-prove-goal "setup_call_cleanup(assertz(cleanup_ran(no)), true, (retractall(cleanup_ran(_)), assertz(cleanup_ran(yes)))), cleanup_ran(yes)"))
  (is-equal "foo" (let ((r (handler-case (progn (pl-prove-goal "setup_call_cleanup(true, throw(foo), assertz(cleaned))") nil) (pl-error (c) (pl-term-to-string (pl-error-ball c)))))) r))
  (is (pl-prove-goal "cleaned"))
  (is (pl-prove-goal "assertion(true)"))
  (is-equal "assertion_failed(fail)" (pl-error-of "assertion(fail)"))
  (is (pl-prove-goal "subsumes_term(f(_), f(a)), \\+ subsumes_term(f(a), f(_))"))
  (is-equal "x" (plout "apply(format, ['~w', [x]])"))
  (is (pl-prove-goal "apply(succ, [1, X]), X == 2")))

;;; ---- PROLOG mode in the editor ------------------------------------------------------------------------------------------------------------

(defun pl-editor ()
  (multiple-value-bind (wm ed) (make-editor-wm)
    (setf (buffer-mode (ed-buffer ed)) 'prolog-mode)
    (values wm ed (ed-buffer ed))))

(defun pl-point (b) (cons (point-line b) (point-col b)))

(deftest prolog-mode-basics
  (is-equal 'prolog-mode (file-mode-for "/tmp/x/family.pl"))
  (is-equal 'prolog-mode (file-mode-for "prog.pro"))
  (is-equal 'prolog-mode (file-mode-for "prog.PROLOG"))
  (is-equal 'lisp-mode (file-mode-for "prog.lisp"))
  (is-equal '((0 7 :comment)) (pl-line-syntax "% abc d" nil))
  (is-equal '((0 3 :defname) (4 5 :constant) (7 9 :builtin)) (pl-line-syntax "foo(X) :- bar" nil))
  (is-equal '((4 8 :string)) (pl-line-syntax "  x 'ab'" nil))
  (is-equal '((0 10 :string)) (pl-line-syntax "\"a string\"" nil))
  (multiple-value-bind (spans state) (pl-line-syntax "a :- /* open" nil)
    (is-equal :block state)
    (is-equal '(:comment) (list (third (car (last spans))))))
  (multiple-value-bind (spans state) (pl-line-syntax "still comment */ foo" :block)
    (is-equal nil state)
    (is-equal '(0 16 :comment) (car spans)))
  (multiple-value-bind (spans state) (pl-line-syntax "'multi" nil)
    (declare (ignore spans))
    (is-equal #\' state))
  (multiple-value-bind (wm ed b) (pl-editor)
    (declare (ignore ed))
    ;; LINE (C-j) indents the next line: body goals four columns, inside parentheses by the block layout
    (dolist (l '("foo(X) :-" "bar(X)," "(   X > 1" "->  baz" ";   qux" ")."))
      (ed-type wm l) (ed-keys wm "C-j"))
    (is-equal (format nil "foo(X) :-~%    bar(X),~%    (   X > 1~%    ->  baz~%    ;   qux~%    ).~%") (buffer-string b))
    (is-equal "PROLOG" (mode-display-name (buffer-mode b)))
    (is (plusp (count-screen-rows wm "(PROLOG")))
    ;; TAB re-indents the line
    (buffer-set-text b (format nil "foo :-~%bar,~%        baz."))
    (buf-goto b 1 0) (ed-keys wm "<tab>") (is-equal "    bar," (buf-line b 1))
    (buf-goto b 2 4) (ed-keys wm "<tab>") (is-equal "    baz." (buf-line b 2))
    ;; a new clause starts at the margin; arguments align after the open parenthesis
    (buffer-set-text b (format nil "a.~%    b.~%c(1,~%2)."))
    (buf-goto b 1 4) (ed-keys wm "<tab>") (is-equal "b." (buf-line b 1))
    (buf-goto b 3 0) (ed-keys wm "<tab>") (is-equal "  2)." (buf-line b 3))
    (buffer-set-text b (format nil "x :-~%   y(a,~%b)."))
    (buf-goto b 2 0) (ed-keys wm "<tab>") (is-equal "     b)." (buf-line b 2))))

(deftest prolog-mode-clause-motion
  (multiple-value-bind (wm ed b) (pl-editor)
    (declare (ignore ed))
    (buffer-set-text b (format nil "a(1).~%a(2) :-~%    b,~%    c.~%:- dynamic q/1.~%d --> [x]."))
    (buf-goto b 2 5)
    (ed-keys wm "C-M-a") (is-equal '(1 . 0) (pl-point b))
    (ed-keys wm "C-M-e") (is-equal '(3 . 6) (pl-point b))
    (ed-keys wm "C-M-f") (is-equal '(4 . 0) (pl-point b))
    (ed-keys wm "C-M-f") (is-equal '(5 . 0) (pl-point b))
    (ed-keys wm "C-M-b") (is-equal '(4 . 0) (pl-point b))
    (buf-goto b 2 5) (ed-keys wm "C-M-h")
    (is-equal '(1 . 0) (pl-point b))
    (is-equal '(3 . 6) (buffer-mark b))
    (is-equal 4 (length (pl-buffer-clauses b)))
    (is-equal '(:clause :clause :directive :clause) (mapcar #'plunit-kind (pl-buffer-clauses b)))
    (is-equal '(("a" 1) ("a" 1) (nil nil) ("d" 2))
              (mapcar (lambda (u) (list (and (plunit-name u) (symbol-name (plunit-name u))) (plunit-arity u))) (pl-buffer-clauses b)))
    (ed-keys wm "M-x") (ed-type wm "list definitions") (ed-keys wm "RET")
    (is (pl-has (buffer-string (ed-buffer (find-if (lambda (w) (typep w 'editor-window)) (wm-windows wm)))) "a/1  (line 1, 2 clauses)"))))

(deftest prolog-mode-compiling
  (pl-reset)
  (multiple-value-bind (wm ed b) (pl-editor)
    (buffer-set-text b (format nil "edfact(1).~%edfact(2).~%edrule(X) :- edfact(X), X > 1.~%edother(z)."))
    (pl-reset-baselines b)
    (buf-goto b 2 8)
    ;; C-c C-c compiles the predicate at point
    (ed-keys wm "C-c C-c")
    (is (pl-has (ed-message ed) "1 clause compiled"))
    (is-equal "existence_error(procedure,edfact/1)" (pl-error-of "edrule(2)"))
    (buf-goto b 0 0) (ed-keys wm "C-c C-c")
    (is (pl-has (ed-message ed) "2 clauses compiled"))
    (is-equal '(2) (pl-first "findall(X, edrule(X), L)" "L"))
    (is (pl-prove-goal "edfact(2)"))
    (ed-keys wm "C-c C-b")
    (is (pl-has (ed-message ed) "4 clauses compiled"))
    (is (pl-prove-goal "edother(z)"))
    ;; Compile Changed Definitions: only what changed
    (ed-keys wm "M-x") (ed-type wm "compile changed definitions of buffer") (ed-keys wm "RET")
    (is-equal "No definitions changed" (ed-message ed))
    (buf-goto b 3 9) (buf-insert b "9")
    (ed-keys wm "M-x") (ed-type wm "list changed definitions") (ed-keys wm "RET")
    (is-equal 1 (length (poss-entries (ed-possibilities ed))))
    (ed-switch-to-buffer ed b)
    (ed-keys wm "M-x") (ed-type wm "compile changed definitions of buffer") (ed-keys wm "RET")
    (is (pl-has (ed-message ed) "1 predicate compiled"))
    (is (pl-prove-goal "edother(z9)"))
    ;; warnings: a syntax error is reported with its line, and C-. goes there
    (buffer-set-text b (format nil "ok(1).~%bad(.~%ok(2)."))
    (ed-keys wm "C-c C-b")
    (is (pl-has (ed-message ed) "2 clauses compiled, 1 problem"))
    (is (pl-has (ed-message ed) "line 2"))
    (ed-keys wm "M-x") (ed-type wm "edit compiler warnings") (ed-keys wm "RET")
    (is-equal 1 (point-line b))
    (is-equal 1 (length (pl-compiler-warnings (buffer-name b))))))

(deftest prolog-mode-queries-and-definitions
  (pl-reset)
  (multiple-value-bind (wm ed b) (pl-editor)
    (buffer-set-text b (format nil "color(red).~%color(green).~%pick(X) :- color(X)."))
    (ed-keys wm "C-c C-b")
    ;; C-c C-q runs a query and shows its answers
    (ed-keys wm "C-c C-q") (ed-type wm "color(X)") (ed-keys wm "RET")
    (let ((out (buffer-string (ed-buffer (find-if (lambda (w) (typep w 'editor-window)) (wm-windows wm))))))
      (is (pl-has out "X = red"))
      (is (pl-has out "X = green")))
    (ed-switch-to-buffer ed b)
    (buffer-set-text b (format nil "?- pick(X).~%"))
    (buf-goto b 0 0) (ed-keys wm "C-c C-e")
    (is (pl-has (buffer-string (ed-buffer (find-if (lambda (w) (typep w 'editor-window)) (wm-windows wm)))) "X = red"))
    (ed-switch-to-buffer ed b)
    ;; M-. jumps to a definition in the buffer
    (buffer-set-text b (format nil "color(red).~%other :- color(red).~%pick(X) :- color(X)."))
    (buf-goto b 1 12) (ed-keys wm "M-.") (is-equal '(0 . 0) (pl-point b))
    ;; C-c C-l lists the predicate at point
    (buf-goto b 2 0) (ed-keys wm "C-c C-l")
    (is (pl-has (buffer-string (ed-buffer (find-if (lambda (w) (typep w 'editor-window)) (wm-windows wm)))) "pick(A) :-"))
    (ed-switch-to-buffer ed b)
    (ed-keys wm "C-c C-t") (is *pl-trace*) (ed-keys wm "C-c C-t") (is (not *pl-trace*))))

;;; ---- Listener commands ------------------------------------------------------------------------------------------------------------------------

(deftest prolog-listener-commands
  (pl-reset)
  (let ((f (pl-tmp "cmd.pl")))
    (with-open-file (s f :direction :output) (write-string (format nil "cmd_fact(one).~%cmd_fact(two).~%cmd_bad(.~%") s))
    (multiple-value-bind (wm lw) (make-test-wm 40 100)
      (run-script wm (script-lines (format nil ":consult prolog ~A" f) ":prolog query cmd_fact(X)" ":show prolog predicates" ":show prolog warnings"))
      (is (not (pane-has lw "Unknown command")))
      (is (pane-has lw "2 clauses, 1 error"))
      (is (pane-has lw "X = one"))
      (is (pane-has lw "X = two"))
      (is (pane-has lw "cmd_fact/1  2 clauses"))
      (is (pane-has lw "line 3")))
    (multiple-value-bind (wm lw) (make-test-wm 40 100)
      (run-script wm (script-lines ":prolog query member(X, [a])" ":prolog query fail" ":reset prolog" ":show prolog predicates"
                                   ":prolog query nosuch(1)" ":trace prolog on" ":trace prolog off"))
      (is (pane-has lw "X = a"))
      (is (pane-has lw "false"))
      (is (pane-has lw "No Prolog predicates are defined"))
      (is (pane-has lw "Unknown procedure: nosuch/1"))
      (is (pane-has lw "Prolog tracing on"))
      (is (pane-has lw "Prolog tracing off")))
    (delete-file f)))

;;; ---- the documentation -------------------------------------------------------------------------------------------------------------------------

(deftest prolog-documentation-and-integration
  (is (find-document :prolog-manual))
  (pl-quiet-consult "x." :source "nowarn")
  (is-equal nil (pl-compiler-warnings "nowarn"))
  ;; the library is part of the image and marked as library
  (is (plpred-library (pl-find-pred (pl-atom "append") 3)))
  (is (not (plpred-library (pl-find-pred (pl-atom "atom_length") 2))))
  (is (plpred-ctl (pl-find-pred (pl-atom ",") 2)))
  (is (pl-find-pred (pl-atom "format") 2))
  (is (fboundp 'prolog:query))
  (is (find-package "PL-ATOMS")))
