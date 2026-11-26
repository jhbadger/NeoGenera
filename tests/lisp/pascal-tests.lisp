;;;; pascal-tests.lisp -- the Pascal Tool Kit: compiler, run-time library, units, systems, the Listener and
;;;; Pascal mode in the editor.  Uses helpers from ui-tests.lisp, tools-tests.lisp and debugger-tests.lisp.

(in-package :cl)

(defun pas-run (source &rest options)
  "Compile SOURCE, run its main program and return what it wrote."
  (let ((out (make-string-output-stream)))
    (apply #'pascal-run-string source :output out options)
    (get-output-stream-string out)))

(defun pas-input (text) (make-string-input-stream text))

(defun pas-error (source &rest options)
  "The message of the run-time error running SOURCE signals (or NIL)."
  (handler-case (progn (apply #'pas-run source options) nil)
    (pascal-runtime-error (e) (princ-to-string e))))

(defun pas-compile-errors (source &rest options)
  (nth-value 1 (apply #'pascal-compile-string source options)))

(defun pas-has (text part) (and (%string-search part text 0) t))

(defun pas-tmp (name) (format nil "/tmp/ng-pascal-~D-~D-~A" (get-universal-time) (random 1000000) name))

(defparameter *pas-factor*
  "PROGRAM factor;
VAR x : integer;
  function factorial (n : integer) : integer;
  VAR i : integer;
  BEGIN
    factorial := 1;
    FOR i := n DOWNTO 1 DO
      factorial := factorial * i
  END; { Function factorial }
BEGIN
  REPEAT
    write ('Find the factorial of what number? ');
    readln(x);
    writeln('Factorial ',x:1,' is ',factorial(x):1)
  UNTIL x = 0
END.")

(defparameter *pas-quadratic*
  "PROGRAM quadratic;
VAR a,b,c,discriminant,x1,x2 : real;
BEGIN
  readln(a,b,c);
  discriminant := b*b - 4*a*c;
  x1 := (-b + sqrt(discriminant)) / (2*a);
  x2 := (-b - sqrt(discriminant)) / (2*a);
  writeln(x1,' and ',x2)
END.")

;;; ---- the front end -----------------------------------------------------------------------------------------

(deftest pascal-lexer-and-parser
  (let ((toks (pascal-tokenize "x := 'it''s' + 3.5e2; { c } (* d *) y[1..2]^")))
    (is-equal '(:id :sym :str :sym :real :sym :id :sym :int :sym :int :sym :sym :eof) (mapcar #'tok-kind toks))
    (is-equal "it's" (tok-value (nth 2 toks)))
    (is-equal 350.0 (tok-value (nth 4 toks))))
  (is-equal '(:kw :id) (mapcar #'tok-kind (butlast (pascal-tokenize "Begin leave"))))               ; LEAVE is reserved only in VS
  (is-equal '(:kw :kw) (mapcar #'tok-kind (butlast (pascal-tokenize "Begin leave" :dialect :vs))))
  (is-equal :template (tok-kind (car (pascal-tokenize "⊂statement⊃"))))
  (multiple-value-bind (units errors) (pascal-parse "program p; begin x := ; y := 3 + ; end.")
    (is-equal 1 (length units))
    (is-equal 2 (length errors))                                         ; recovery finds both errors
    (is-equal '(1 23) (list (pascal-error-line (car errors)) (pascal-error-col (car errors)))))
  (multiple-value-bind (units errors) (pascal-parse *pas-factor*)
    (is-equal nil errors)
    (is-equal :program (pnode-kind (car units))))
  (is-equal '(:mode "PASCAL" :dialect "VS" :package "MYPKG")
            (let ((a (pascal-file-attributes "{-*- Mode: PASCAL; Dialect: VS; Package: MYPKG -*- }"))) (list :mode (getf a :mode) :dialect (getf a :dialect) :package (getf a :package)))))

;;; ---- running programs -----------------------------------------------------------------------------------------

(deftest pascal-guide-examples
  (is-equal "Find the factorial of what number? Factorial 5 is 120
Find the factorial of what number? Factorial 30 is 265252859812191058636308480000000
Find the factorial of what number? Factorial 0 is 1
"
            (pas-run *pas-factor* :input (pas-input (format nil "5~%30~%0~%"))))
  (is-equal (format nil " 3.000000000000E+000 and -3.000000000000E+000~%") (pas-run *pas-quadratic* :input (pas-input "1 0 -9")))
  (let ((msg (pas-error *pas-quadratic* :input (pas-input "1 2 3"))))
    (is (pas-has msg "Attempt to take the square root of -8.0, which is negative"))
    (is (pas-has msg "in Pascal routine quadratic, line 6")))
  ;; first-level routines are Lisp functions NAME<-PROGRAM
  (pascal-compile-string *pas-factor*)
  (is-equal 3628800 (funcall (intern "FACTORIAL<-FACTOR" "PASCAL-USER") 10))
  (is (member (intern "FACTOR" "PASCAL-USER") (pascal-programs)))
  (is-equal (format nil "Find the factorial of what number? Factorial 0 is 1~%")
            (let ((out (make-string-output-stream))) (pascal-execute "pascal-user:factor" :input (pas-input "0") :output out) (get-output-stream-string out)))
  (is (eq (symbol-function (intern "EXECUTE" "PASCAL")) #'pascal-execute)))

(deftest pascal-data-types
  (is-equal (format nil "~{~A~%~}" '("p.x=3 q.x=99" "with 7" "list 15" "1 5 6 7 8 9 50 1000 " "green blue 1" "circle   2.5" "23 31" "TRUE FALSE"))
            (pas-run "program types;
type color = (red, green, blue);
     point = record x, y : integer end;
     list = ^node;
     node = record value : integer; next : list end;
     shape = record name : packed array[1..8] of char;
                    case kind : color of red : (radius : real); green, blue : (w, h : integer) end;
var p, q : point; head, t : list; i, total : integer; s : set of 0..2000; c : color; sh : shape;
    m : array[1..3, 1..3] of integer;
begin
  p.x := 3; p.y := 4; q := p; q.x := 99; writeln('p.x=', p.x:1, ' q.x=', q.x:1);
  with p do writeln('with ', x + y:1);
  head := nil;
  for i := 1 to 5 do begin new(t); t^.value := i; t^.next := head; head := t end;
  total := 0; t := head; while t <> nil do begin total := total + t^.value; t := t^.next end;
  writeln('list ', total:1);
  s := [1, 3, 5..9] + [50, 1000] - [3];
  for i := 0 to 2000 do if i in s then write(i:1, ' '); writeln;
  c := green; writeln(c, ' ', succ(c), ' ', ord(c):1);
  sh.name := 'circle'; sh.kind := red; sh.radius := 2.5; writeln(sh.name, sh.radius:4:1);
  for i := 1 to 3 do m[i, 1] := i * 10 + 1; m[2][3] := 23; writeln(m[2, 3]:1, ' ', m[3][1]:1);
  writeln([1, 2] <= [1, 2, 3], ' ', 'abc' > 'abd')
end."))
  ;; integers never overflow; reals and their formats
  (is-equal (format nil "19807040600895968300706562046~%3 -1 3.50 -3 3 A  2.25~% 1.0E+000|    3.14159| 1.23457E+002~%")
            (pas-run "program nums; begin
  writeln(2*maxint*maxint*maxint:1);
  writeln(17 div 5:1, ' ', -7 mod 3:1, ' ', 7/2:4:2, ' ', round(-2.5):1, ' ', trunc(3.7):1, ' ', chr(65), ' ', sqr(1.5):5:2);
  writeln(1.0:8, '|', 3.14159:11:5, '|', 123.45678:13)
end.")))

(deftest pascal-routines
  (is-equal (format nil "~{~A~%~}" '("2 1" "6765" "count 3" "apply 42" "TRUE TRUE" "  15.0  30.0" "385"))
            (pas-run "program routines;
type vec = array[1..10] of integer; v5 = array[1..5] of real; v3 = array[0..2] of real;
var i, j : integer; a : v5; b : v3; v : vec;
procedure swap(var a, b : integer); var t : integer; begin t := a; a := b; b := t end;
function fib(k : integer) : integer; begin if k < 2 then fib := k else fib := fib(k-1) + fib(k-2) end;
procedure counter; var count : integer;
  procedure bump; begin count := count + 1 end;
begin count := 0; bump; bump; bump; writeln('count ', count:1) end;
function apply(function f(x : integer) : integer; x : integer) : integer; begin apply := f(x) end;
function double(x : integer) : integer; begin double := 2 * x end;
function even(n : integer) : boolean; forward;
function oddn(n : integer) : boolean; begin if n = 0 then oddn := false else oddn := even(n - 1) end;
function even; begin if n = 0 then even := true else even := oddn(n - 1) end;
function total(var x : array[lo..hi : integer] of real) : real; var k : integer; s : real;
begin s := 0; for k := lo to hi do s := s + x[k]; total := s end;
function sum(a : vec) : integer; var k : integer; begin sum := 0; for k := 1 to 10 do sum := sum + a[k]; a[1] := -1 end;
begin
  i := 1; j := 2; swap(i, j); writeln(i:1, ' ', j:1);
  writeln(fib(20):1);
  counter;
  writeln('apply ', apply(double, 21):1);
  writeln(even(10), ' ', oddn(7));
  for i := 1 to 5 do a[i] := i; for i := 0 to 2 do b[i] := 10 * i; writeln(total(a):6:1, total(b):6:1);
  for i := 1 to 10 do v[i] := i * i; i := sum(v); if v[1] = 1 then writeln(i:1)
end.")))

(deftest pascal-statements
  (is-equal (format nil "~{~A~%~}" '("a or b" "other" "goto 3" "done" "repeat 4" "zyx" "halting"))
            (pas-run "program stmts;
label 10, 99;
var i : integer; ch : char;
begin
  ch := 'a'; case ch of 'a', 'b': writeln('a or b'); 'c': writeln('c') otherwise writeln('other') end;
  ch := 'q'; case ch of 'a', 'b': writeln('a or b'); 'c': writeln('c') otherwise writeln('other') end;
  i := 0;
  10: i := i + 1;
  if i < 3 then goto 10;
  writeln('goto ', i:1);
  goto 99;
  writeln('skipped');
  99: writeln('done');
  i := 0; repeat i := i + 1 until i * i > 10; writeln('repeat ', i:1);
  for ch := 'z' downto 'x' do write(ch); writeln;
  writeln('halting'); halt; writeln('not reached')
end.")))

(deftest pascal-input-and-files
  (is-equal (format nil "7   2.5 [rest of line]|eoln TRUE~%eof TRUE~%")                     ; the last read(c) reads the blank
            (pas-run "program inp; var n : integer; c : char; r : real; s : packed array[1..14] of char;
begin read(n); read(c); read(c); read(r); read(c); read(s); writeln(n:1, ' ', c, ' ', r:3:1, ' ', s, '|eoln ', eoln); readln; writeln('eof ', eof) end."
                     :input (pas-input (format nil "  7 x 2.5 [rest of line]~%"))))
  (let ((txt (pas-tmp "t.txt")) (dat (pas-tmp "t.dat")))
    (is-equal (format nil "total 20 line abcde~%binary 1000~%")
              (pas-run (format nil "program files; var f : text; g : file of integer; i, x, total : integer; line : packed array[1..5] of char;
begin rewrite(f, 'name=~A'); for i := 1 to 3 do writeln(f, i:1, ' ', i*i:1); writeln(f, 'abcde'); close(f);
  reset(f, 'name=~A'); total := 0;
  for i := 1 to 3 do begin read(f, x); total := total + x; readln(f, x); total := total + x end;
  read(f, line); writeln('total ', total:1, ' line ', line);
  rewrite(g, 'name=~A'); for i := 1 to 4 do write(g, i * 100); close(g);
  reset(g, 'name=~A'); total := 0; while not eof(g) do begin read(g, x); total := total + x end; writeln('binary ', total:1) end." txt txt dat dat)))
    (is (probe-file txt))
    (delete-file txt) (delete-file dat))
  ;; :INPUT and :OUTPUT may be file names
  (let ((in (pas-tmp "in.txt")) (out (pas-tmp "out.txt")))
    (with-open-file (s in :direction :output) (format s "6~%0~%"))
    (pascal-compile-string *pas-factor*)
    (pascal-execute "factor" :input in :output out)
    (is (pas-has (%read-file-string out) "Factorial 6 is 720"))
    (delete-file in) (delete-file out)))

(deftest pascal-runtime-checks
  (is (pas-has (pas-error "program r; var x, y : integer; begin y := x + 1 end.") "The variable x is used before it has been given a value"))
  (is-equal (format nil "1~%") (pas-run "program r; var x, y : integer; begin y := x + 1; writeln(y:1) end." :init-to-zero t))
  (is (pas-has (pas-error "program r; var d : 1..10; begin d := 5; d := d * 3 end.") "The value 15 is out of the range 1..10"))
  (is (pas-has (pas-error "program r; var a : array[1..3] of integer; i : integer; begin i := 4; a[i] := 1 end.") "Index 4 is out of the array bounds 1..3"))
  (is (pas-has (pas-error "program r; type p = ^integer; var x : p; begin x := nil; x^ := 3 end.") "Attempt to follow the pointer NIL"))
  (is (pas-has (pas-error "program r; type p = ^integer; var x, y : p; begin new(x); y := x; dispose(x); y^ := 1 end.") "disposed"))
  (is (pas-has (pas-error "program r; var x : integer; begin x := 5; case x of 1: x := 2 end end.") "No CASE label matches the value 5"))
  (let ((m (pas-error "program r;
var x : integer;
function f(k : integer) : integer;
begin
  f := 10 div k
end;
begin x := f(0) end.")))
    (is (pas-has m "Division by zero"))
    (is (pas-has m "in Pascal routine f, line 5")))
  (is (pas-has (pas-error "program r; var x : integer; function f(k : integer) : integer; begin if k > 0 then f := 1 end; begin x := f(0) end.")
               "returned without assigning its result"))
  ;; an error signalled by Lisp gets the Pascal context too
  (is (pas-has (pas-error "program r; var l : lispobject; function car(x : lispobject) : integer; lisp 'cl:car'; begin l := nil; writeln(car(l) + 1:1) end.")
               "in Pascal routine r")))

(deftest pascal-compile-errors
  (let ((errs (pas-compile-errors "program e;
var x : integer; b : boolean;
begin
  x := 'abc';
  b := x + 1;
  y := 3;
  if x then x := 1;
  10: x := 2
end.")))
    (is-equal '(4 5 6 7 8) (mapcar #'pascal-error-line errs))
    (is (pas-has (pascal-error-message (first errs)) "Type mismatch"))
    (is (pas-has (pascal-error-message (third errs)) "y is not declared"))
    (is (pas-has (pascal-error-message (fourth errs)) "condition must be a boolean")))
  (is (pas-compile-errors "program x; var s : string(10); begin end."))                       ; STRING is Pascal/VS
  (is (pas-compile-errors "program x; var i : integer; procedure p(a : integer); begin end; begin p(1, 2) end."))
  (is (pas-compile-errors "program x; var a : array[1..3] of integer; begin a[4] := 0 end."))  ; a constant subscript is checked now
  (is (pas-compile-errors "program x; function f : integer; forward; begin end."))
  ;; the warnings database keeps the errors of the last compilation of each source
  (pascal-compile-string "program w; begin x := 1 end." :warnings-key "test-source")
  (is-equal 1 (length (pascal-compiler-warnings "test-source")))
  (pascal-compile-string "program w; begin end." :warnings-key "test-source")
  (is-equal nil (pascal-compiler-warnings "test-source")))

(deftest pascal-dialects
  (is-equal (format nil "Hello, world 12~%Hello|8|ab|a b c|~%Jello~% 2 4~%")
            (pas-run "program strs; var s, t : string(40); i : integer;
begin s := 'Hello'; t := s || ', world'; writeln(t, ' ', length(t):1);
  writeln(substr(t, 1, 5), '|', index(t, 'world'):1, '|', trim('ab   '), '|', compress('a    b  c'), '|');
  s[1] := 'J'; writeln(s);
  i := 0; while true do begin i := i + 1; if i > 5 then leave; if odd(i) then continue; write(i:2) end; writeln end." :dialect :vs))
  (is (pas-has (pas-error "program s; var s : string(3); begin s := 'abcd' end." :dialect :vs) "longer than its maximum length 3")))

(deftest pascal-units-and-lisp
  (pascal-compile-string "unit_specification mathu; type int = integer; var calls : integer; function factorial(n : int) : int; end.")
  (pascal-compile-string "unit_implementation mathu; function factorial; var i : integer;
   begin calls := calls + 1; factorial := 1; for i := n downto 1 do factorial := factorial * i end; end.")
  (is-equal (format nil "25! = 15511210043330985984000000 calls 1~%")
            (pas-run "program usesmath; using_units mathu; var v : int; begin calls := 0; v := factorial(25); writeln('25! = ', v:1, ' calls ', calls:1) end."))
  (is-equal '("MATHU") (pascal-units-depended-on "USESMATH"))
  (is (member "USESMATH" (pascal-dependent-units "MATHU") :test #'string=))
  (is (pas-compile-errors "unit_implementation nosuchunit; end."))
  ;; the LISP routine directive and lispobject
  (is-equal (format nil "(10 9 8 7 6 5 4 3 2 1) 10~%")
            (pas-run "program revarray; type intarray = array[1..10] of integer; var a : intarray; l : lispobject; i : integer;
  function listarray(var arr : intarray) : lispobject; lisp 'cl:pas-vector-list';
  function reverse(l : lispobject) : lispobject; lisp 'global:reverse';
  function len(l : lispobject) : integer; lisp 'cl:length';
  begin for i := 1 to 10 do a[i] := i; l := reverse(listarray(a)); writeln(l, ' ', len(l):1) end.")))

(defun pas-vector-list (v) (coerce v 'list))

(deftest pascal-incremental-and-environments
  (is-equal (format nil "2~%") (pas-run "program inc; function f(x : integer) : integer; begin f := x + 1 end; begin writeln(f(1):1) end."))
  ;; recompiling just the routine at an offset
  (is-equal (list (intern "F<-INC" "PASCAL-USER"))
            (pascal-compile-string "program inc; function f(x : integer) : integer; begin f := x + 100 end; begin writeln(f(1):1) end." :only-at 40))
  (is-equal (format nil "101~%") (let ((out (make-string-output-stream))) (pascal-execute "inc" :output out) (get-output-stream-string out)))
  ;; :SAVE-ENVIRONMENT keeps the globals between runs
  (pascal-compile-string "program tally; var n : integer; begin if n = 0 then n := 0; n := n + 1; writeln(n:1) end.")
  (flet ((run (&rest o) (let ((out (make-string-output-stream))) (apply #'pascal-execute "tally" :output out :init-to-zero t o) (get-output-stream-string out))))
    (is-equal (format nil "1~%") (run :save-environment t))
    (is-equal (format nil "2~%") (run :save-environment t))
    (is-equal (format nil "1~%") (run :save-environment t :reload t))
    (is-equal (format nil "1~%") (run))))

(deftest pascal-files-and-systems
  (let* ((dir (format nil "/tmp/ng-pascal-sys-~D-~D/" (get-universal-time) (random 1000000)))
         (spec (%string-concat dir "stack.pas")) (main (%string-concat dir "main.pas")))
    (ensure-directories-exist dir)
    (with-open-file (s spec :direction :output)
      (write-string "{-*- Mode: PASCAL; Dialect: ISO -*- }
unit_specification stacks;
type stack = record top : integer; items : array[1..100] of integer end;
procedure init(var s : stack); procedure push(var s : stack; x : integer); function pop(var s : stack) : integer;
end.
unit_implementation stacks;
procedure init; begin s.top := 0 end;
procedure push; begin s.top := s.top + 1; s.items[s.top] := x end;
function pop; begin pop := s.items[s.top]; s.top := s.top - 1 end;
end.
" s))
    (with-open-file (s main :direction :output)
      (write-string "program rpn; using_units stacks; var s : stack; i : integer;
begin init(s); for i := 1 to 5 do push(s, i * i); write('popped:'); while s.top > 0 do write(' ', pop(s):1); writeln end.
" s))
    (eval `(def-program pas-rpn-demo (:default-pathname ,dir :default-package "RPNPKG") (:using "stack" "main")))
    (is-equal '("stack.pas" "main.pas") (mapcar #'file-namestring (system-files 'pas-rpn-demo)))
    (compile-system 'pas-rpn-demo :verbose nil)
    (is (probe-file (%string-concat dir "main.nfasl")))
    (is-equal (format nil "popped: 25 16 9 4 1~%") (let ((out (make-string-output-stream))) (pascal-execute "rpnpkg:rpn" :output out) (get-output-stream-string out)))
    (is (find-package "RPNPKG"))
    (is (pascal-source-file-p main))
    (%run-program "rm" (list "-rf" dir))))

(deftest pascal-in-the-listener
  (pascal-compile-string "program greet; var name : packed array[1..5] of char; n, m : integer;
begin write('Your name? '); readln(name); write('Two numbers? '); readln(n, m); writeln('Hello, ', name, ' sum=', n + m:1) end.")
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines "(pascal-execute 'pascal-user::greet)" "Alice" "3 4"))
    (is (pane-has lw "Your name? Alice"))
    (is (pane-has lw "Hello, Alice sum=7")))
  ;; a run-time error enters the debugger, which names the Pascal routine and line
  (pascal-compile-string "program sq; var a, b : real; begin readln(a, b); writeln(sqrt(a - b)) end.")
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((levels nil))
      (run-script wm (script-lines "(pascal-execute 'pascal-user::sq)" "1 2") (at (lambda () (setq levels (length (lw-debug lw))))) (script-lines "0"))
      (is-equal 1 levels)
      (is (pane-has lw "Attempt to take the square root of -1.0"))
      (is (pane-has lw "in Pascal routine sq, line 1"))))
  ;; the Execute Pascal command
  (pascal-compile-string "program hello; begin writeln('hello from Pascal') end.")
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines ":execute pascal hello no" ":show pascal programs"))
    (is (pane-has lw "hello from Pascal"))
    (is (pane-has lw "PASCAL-USER:hello"))))

;;; ---- Pascal mode ---------------------------------------------------------------------------------------------

(defun pas-editor ()
  (multiple-value-bind (wm ed) (make-editor-wm)
    (setf (buffer-mode (ed-buffer ed)) 'pascal-mode)
    (values wm ed (ed-buffer ed))))

(defun pas-point (b) (cons (point-line b) (point-col b)))

(defparameter *pas-demo-buffer* "PROGRAM demo;
VAR i : integer;
BEGIN
  FOR i := 1 TO 3 DO
    writeln(i);
  IF 2 > 1 THEN
  BEGIN
    writeln('big')
  END
END.")

(deftest pascal-mode-basics
  (is-equal 'pascal-mode (file-mode-for "/tmp/x/quadratic.pascal"))
  (is-equal 'pascal-mode (file-mode-for "prog.pas"))
  (is-equal 'lisp-mode (file-mode-for "prog.lisp"))
  (is-equal '((0 7 :builtin) (8 14 :defname) (16 27 :comment) (28 31 :builtin) (36 43 :keyword) (45 50 :builtin) (51 58 :keyword) (59 63 :string) (65 69 :constant) (71 74 :builtin))
            (pascal-line-syntax "PROGRAM factor; { comment } var x : integer; begin writeln('hi', true) end." nil))
  (multiple-value-bind (spans state) (pascal-line-syntax "x := 1; (* open" nil)
    (is-equal :paren state)
    (is-equal :comment (caddr (car (last spans)))))
  (multiple-value-bind (wm ed b) (pas-editor)
    ;; Electric Pascal mode and indentation, typing a program with LINE (C-j)
    (ed-keys wm "M-x") (ed-type wm "electric pascal mode") (ed-keys wm "RET")
    (is (member 'electric (buffer-minor-modes b)))
    (dolist (l '("program demo;" "var i : integer;" "begin" "for i := 1 to 3 do" "writeln(i);" "if 2 > 1 then" "begin" "writeln('big')"))
      (ed-type wm l) (ed-keys wm "C-j"))
    (ed-type wm "end") (ed-keys wm "<tab> C-j")
    (ed-type wm "end.")
    (ed-keys wm "<tab>")
    (is-equal *pas-demo-buffer* (buffer-string b))
    (is (plusp (count-screen-rows wm "(Pascal ELECTRIC)")))
    ;; c-sh-C compiles the program (point is in its body)
    (ed-keys wm "C-c C-c")
    (is-equal "program demo compiled" (ed-message ed))
    (is-equal (format nil "         1~%         2~%         3~%big~%")
              (let ((out (make-string-output-stream))) (pascal-execute "demo" :output out) (get-output-stream-string out)))
    ;; LINE reports a syntax error in the line it ends
    (buffer-set-text b "program e; var j, k : integer; begin") (buf-goto b 0 (length (buf-line b 0)))
    (ed-keys wm "C-j") (ed-type wm "j := k + ;") (ed-keys wm "C-j")
    (is (pas-has (ed-message ed) "Expression expected"))))

(deftest pascal-mode-language-units
  (multiple-value-bind (wm ed b) (pas-editor)
    (declare (ignore ed))
    (buffer-set-text b *pas-demo-buffer*)
    (buf-goto b 4 6)
    (ed-keys wm "C-M-f") (is-equal '(4 . 14) (pas-point b))                ; end of writeln(i)
    (ed-keys wm "C-M-b") (is-equal '(4 . 4) (pas-point b))
    (ed-keys wm "C-M-b") (is-equal '(3 . 2) (pas-point b))                 ; the FOR statement
    (ed-keys wm "C-M-a") (is-equal '(0 . 0) (pas-point b))
    (ed-keys wm "C-M-e") (is-equal '(9 . 4) (pas-point b))
    ;; deleting statements: C-c k kills the statement on the line (the whole FOR)
    (buf-goto b 3 2) (ed-keys wm "C-c k")
    (is-equal "  ;" (buf-line b 3))
    (ed-keys wm "C-y") (is-equal "  FOR i := 1 TO 3 DO" (buf-line b 3))
    ;; expressions and tokens
    (buffer-set-text b "while (A + B +(B+ B )) do x := 1") (buf-goto b 0 6)
    (ed-keys wm "C-c f") (is-equal '(0 . 22) (pas-point b))
    (ed-keys wm "C-c b") (is-equal '(0 . 6) (pas-point b))
    (buf-goto b 0 14) (ed-keys wm "C-c x") (is-equal "while (A + B +) do x := 1" (buf-line b 0))   ; at the second "(": (B+ B )
    (buf-goto b 0 0) (ed-keys wm "C-c t") (is-equal " (A + B +) do x := 1" (buf-line b 0))
    ;; syntax errors
    (buffer-set-text b "program e;
var j, k : integer;
begin
  j := k + ;
  k := 3 *
end.")
    (buf-goto b 0 0)
    (ed-keys wm "C-c C-n") (is-equal '(3 . 11) (pas-point b))
    (ed-keys wm "C-c C-n") (is-equal '(5 . 0) (pas-point b))
    (ed-keys wm "C-c C-p") (is-equal '(3 . 11) (pas-point b))
    ;; a deletion refuses a construct with a syntax error
    (buf-goto b 3 2) (ed-keys wm "C-M-k") (is-equal "  j := k + ;" (buf-line b 3))))

(deftest pascal-mode-templates-and-completion
  (multiple-value-bind (wm ed b) (pas-editor)
    (buffer-set-text b "program t;
begin
  for
end.")
    (buf-goto b 2 5)
    (ed-keys wm "C-c C-t")                                                     ; END: the template of the keyword
    (is-equal "  FOR ⊂control variable⊃ := ⊂initial value⊃ ⊂to or downto⊃ ⊂final value⊃ DO" (buf-line b 2))
    (is-equal "    ⊂statement⊃" (buf-line b 3))
    (is-equal '(2 . 6) (pas-point b))
    (ed-type wm "i")                                                           ; typing replaces the item
    (ed-keys wm "C-M-n M-<tab>")                                               ; COMPLETE refines ⊂initial value⊃
    (is-equal "  FOR i := ⊂expression⊃ ⊂to or downto⊃ ⊂final value⊃ DO" (buf-line b 2))
    (ed-type wm "1") (ed-keys wm "C-M-n") (ed-type wm "TO") (ed-keys wm "C-M-n") (ed-type wm "10")
    (is-equal "  FOR i := 1 TO 10 DO" (buf-line b 2))
    (ed-keys wm "C-M-n M-<tab>")                                               ; several completions: a menu
    (is (member "Assignment statement" (mapcar #'car (poss-entries (ed-possibilities ed))) :test #'string=))
    (ed-switch-to-buffer ed b)
    (buffer-set-text b "x := ⊂expression⊃;") (buf-goto b 0 5) (ed-keys wm "SPC")       ; SPC at an item deletes it
    (is-equal "x := ;" (buf-line b 0))
    ;; completing reserved words
    (buffer-set-text b "proc") (buf-goto b 0 4) (ed-keys wm "M-<tab>")
    (is-equal "PROCEDURE" (buf-line b 0))
    (buffer-set-text b "pro") (buf-goto b 0 3) (ed-keys wm "M-<tab>")
    (is (pas-has (ed-message ed) "procedure program"))
    (buffer-set-text b "writ") (buf-goto b 0 4) (ed-keys wm "C-c /")
    (is-equal "write writeln" (ed-message ed))
    ;; c-END closes the open constructs
    (buffer-set-text b "begin
  repeat
    x := (a + b")
    (buf-goto b 2 15)
    (ed-keys wm "C-c ] C-c ] C-c ]")
    (is-equal "    x := (a + b)" (buf-line b 2))
    (is-equal "  UNTIL ⊂expression⊃" (buf-line b 3))
    (is-equal "END" (buf-line b 4))
    ;; the construct an END closes
    (let ((*pascal-blink-matching* t))
      (buffer-set-text b "begin x := 1 end") (buf-goto b 0 16)
      (is-equal '(0 1 2 3 4 13 14 15) (sort (pascal-match-offsets b) #'<)))
    ;; Format Language Region and the attribute list
    (buffer-set-text b "program p; Var X : Integer; begin end.")
    (ed-keys wm "C-x h") (ed-keys wm "M-x") (ed-type wm "format language region") (ed-keys wm "RET")
    (is-equal "PROGRAM p; VAR x : integer; BEGIN END." (buf-line b 0))
    (%pascal-set-attributes b :dialect :vs :package "MYPAS")
    (is-equal "{-*- Mode: PASCAL; Dialect: VS; Package: MYPAS -*- }" (buf-line b 0))
    (is-equal :vs (%pascal-buffer-dialect b))
    (is-equal "MYPAS" (%pascal-buffer-package b))))

(deftest pascal-mode-compiler-warnings
  (multiple-value-bind (wm ed b) (pas-editor)
    (buffer-set-text b "program w;
var x : integer;
begin
  x := 'no';
  y := 1
end.")
    (ed-keys wm "C-c C-b")
    (is (pas-has (ed-message ed) "2 compiler warnings; line 4"))
    (buf-goto b 0 0)
    (ed-keys wm "C-.") (is-equal '(3 . 7) (pas-point b))                       ; at the string that does not fit
    (ed-keys wm "C-.") (is-equal '(4 . 2) (pas-point b))
    (buffer-set-text b "program w; var x : integer; begin x := 1 end.")
    (ed-keys wm "C-c C-b")
    (is-equal "program w compiled" (ed-message ed))
    (is-equal nil (pascal-compiler-warnings "work"))))
