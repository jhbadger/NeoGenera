;;; The Pascal Tool Kit: Pascal compiled into Lisp, sharing its integers and its functions.
;;; expect: Factorial 30 is 265252859812191058636308480000000
(pascal-load-file "examples/pascal/factor.pas")
(pascal-load-file "examples/pascal/quadratic.pas")
(pascal-load-file "examples/pascal/sieve.pas")

;; Run main programs; :input gives the standard file INPUT (here a string stream instead of the keyboard).
(pascal-execute 'pascal-user::factor :input (make-string-input-stream (format nil "10~%30~%0~%")))
(pascal-execute 'pascal-user::quadratic :input (make-string-input-stream "1 0 -9"))
(pascal-execute "sieve")

;; Top-level routines are Lisp functions named ROUTINE<-PROGRAM.
(format t "~&From Lisp: 25! = ~D~%" (pascal-user::factorial<-factor 25))

;; Pascal calls Lisp through the LISP routine directive, and holds Lisp data in LISPOBJECTs.
(defun iota-list (n) (loop for i from 1 to n collect i))
(defun list-sum (l) (reduce #'+ l))
(pascal-compile-string "program uselisp;
  var l : lispobject;
  function iota(n : integer) : lispobject; lisp 'cl-user::iota-list';
  function reverse(l : lispobject) : lispobject; lisp 'global:reverse';
  function sum(l : lispobject) : integer; lisp 'cl-user::list-sum';
begin
  l := reverse(iota(10));
  writeln('reversed: ', l, '  sum: ', sum(l):1)
end.")
(pascal-execute "uselisp")

;; Run-time errors name the Pascal routine and line, and enter the debugger in the Listener.
(handler-case (pascal-execute "quadratic" :input (make-string-input-stream "1 2 3"))
  (pascal-runtime-error (e) (format t "~&Error: ~A~%" e)))
