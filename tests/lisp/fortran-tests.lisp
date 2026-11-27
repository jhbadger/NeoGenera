;;;; fortran-tests.lisp -- the FORTRAN Tool Kit: source reading, parser, formatted I/O, compiler, run-time library,
;;;; toolkit and FORTRAN mode in the editor.  Uses helpers from ui-tests.lisp, tools-tests.lisp and debugger-tests.lisp.

(in-package :cl)

;;; packages the tests name in their source (the reader needs them to exist)
(dolist (p '("FTN-FILE" "FTN-OTHER" "FTN-ED" "FTN-SYS" "FTN-MAIN" "NEWFTN" "MYFTN"))
  (package-declare-with-no-superiors p))

(defun ftn-run (source &rest options)
  "Compile SOURCE, run its main program and return what it wrote."
  (let ((out (make-string-output-stream)))
    (apply #'f77-run-string source :output out options)
    (get-output-stream-string out)))

(defun ftn-error (source &rest options)
  "The message of the run-time error running SOURCE signals (or NIL)."
  (handler-case (progn (apply #'ftn-run source options) nil)
    (f77-runtime-error (e) (princ-to-string e))))

(defun ftn-compile-errors (source &rest options)
  "The compile error messages for SOURCE."
  (mapcar #'f77-error-message (nth-value 1 (apply #'f77-compile-string source options))))

(defun ftn-has (text part) (and (%string-search part text 0) t))

(defvar *ftn-tmp-tag* (format nil "~D~D" (mod (get-universal-time) 100000) (mod (get-internal-real-time) 10000))
  "Unique to this run (and short: names go in column-limited FORTRAN source): RANDOM is not seeded, so its names repeat between runs and collide with leftovers.")
(defvar *ftn-tmp-count* 0)
(defun ftn-tmp (name) (format nil "/tmp/nf~A-~D~A" *ftn-tmp-tag* (incf *ftn-tmp-count*) name))

(defun ftn-lines (&rest lines) (format nil "~{~A~%~}" lines))

(defun ftn-kinds (source)
  "The statement kinds of the first program unit of SOURCE."
  (mapcar #'fn-kind (fu-stmts (car (f77-parse source)))))

;;; ---- source reading ------------------------------------------------------------------------------------------

(deftest fortran-fixed-format
  ;; labels, continuation cards, comments, sequence numbers, the column-72 limit
  (let ((stmts (f77-read-statements (ftn-lines "C a comment"
                                               "      X = 1 +"
                                               "     +    2"
                                               "  100 CONTINUE"
                                               "* another comment"
                                               ""
                                               (format nil "~72A~A" "      Y = 3" "SEQ00010")
                                               "      Z = 4 ! trailing comment"))))
    (is-equal 4 (length stmts))
    (is-equal "X=1+2" (%f7-squeeze (fs-text (car stmts))))
    (is-equal 100 (fs-label (cadr stmts)))
    (is-equal 4 (fs-line (cadr stmts)))
    (is-equal "Y=3" (%f7-squeeze (fs-text (caddr stmts))))
    (is-equal "Z=4" (%f7-squeeze (fs-text (cadddr stmts))))
    (is-equal 8 (fs-line (cadddr stmts)))
    ;; a continuation card with nothing to continue, and a bad label field
    (is-equal 1 (length (nth-value 1 (f77-read-statements "     + X = 1"))))
    (is-equal 1 (length (nth-value 1 (f77-read-statements "ab    X = 1"))))
    ;; the offsets of a statement cover all its cards
    (is-equal '(4 . 29) (let ((st (car (f77-read-statements (ftn-lines "C c" "      X = 1 +" "     +    2"))))) (cons (fs-start st) (fs-end st))))))

(deftest fortran-case-and-strings
  ;; blanks are not significant; case is not, except in character constants and Hollerith data
  (is-equal "DOI=1,10" (%f7-squeeze "      do i = 1 , 10"))
  (is-equal "X='Hello World'" (%f7-squeeze "x = 'Hello World'"))
  (is-equal "X='IT''S'" (%f7-squeeze "X = 'IT''S'"))
  (is-equal "X='say \"hi\"'" (%f7-squeeze "X = \"say \"\"hi\"\"\""))
  (is-equal "FORMAT(1X,'hello',I3)" (%f7-squeeze "FORMAT (1X, 5Hhello, i3)"))
  (is-equal "GOTO10" (%f7-squeeze "GO TO 10"))
  (is (f77-read-statements "      X = 'unterminated"))
  (is-equal 1 (length (nth-value 1 (f77-parse "      X = 'unterminated
      END")))))

(deftest fortran-tab-format
  ;; a tab in the first six columns ends the label field; a digit after it is a continuation mark
  (let ((stmts (f77-read-statements (format nil "	X = 1 +~%	1 2~%10	Y = 2~%"))))
    (is-equal 2 (length stmts))
    (is-equal "X=1+2" (%f7-squeeze (fs-text (car stmts))))
    (is-equal 10 (fs-label (cadr stmts)))))

(deftest fortran-tokens
  (is-equal '((:name . "X") (:op . "=") (:int . 1) (:op . "+") (:real . 2.5) (:op . "*") (:dreal . 1.0d3))
            (f77-tokenize "X=1+2.5*1.0D3"))
  ;; 1.EQ.2 is an integer, an operator and an integer
  (is-equal '((:int . 1) (:dot . :eq) (:int . 2)) (f77-tokenize "1.EQ.2"))
  (is-equal '((:logical . t) (:dot . :and) (:logical . nil)) (f77-tokenize ".TRUE..AND..FALSE."))
  (is-equal '((:int . 255) (:int . 5) (:int . 8) (:int . 15)) (f77-tokenize "Z'FF'B'101'O'10'X'F'"))
  (is-equal '((:str . "it's")) (f77-tokenize "'it''s'"))
  (is-equal '((:name . "A") (:op . "**") (:op . "-") (:name . "B") (:op . "//") (:name . "C")) (f77-tokenize "A**-B//C")))

;;; ---- the parser --------------------------------------------------------------------------------------------------

(deftest fortran-parser-statements
  (is-equal '(:program :type :dimension :common :equivalence :parameter :data :do :assign :continue :dowhile :enddo :if :print :elseif
              :write :else :call :endif :aif :lif :cgoto :format :end)
            (ftn-kinds (ftn-lines "      PROGRAM T"
                                  "      INTEGER I, N(10)"
                                  "      DIMENSION A(5)"
                                  "      COMMON /BLK/ A1"
                                  "      EQUIVALENCE (A1, B1)"
                                  "      PARAMETER (K=3)"
                                  "      DATA N /10*0/"
                                  "      DO 10 I = 1, 10"
                                  "      X = X + I**2"
                                  "   10 CONTINUE"
                                  "      DO WHILE (X .GT. 0)"
                                  "      ENDDO"
                                  "      IF (X .LT. 0) THEN"
                                  "      PRINT *, 'NEG'"
                                  "      ELSE IF (X .EQ. 0) THEN"
                                  "      WRITE (6, 100) X"
                                  "      ELSE"
                                  "      CALL FOO(X, *10)"
                                  "      END IF"
                                  "      IF (X) 10, 20, 30"
                                  "      IF (X .GT. 1) GOTO 10"
                                  "      GO TO (10, 20, 30), I"
                                  "  100 FORMAT (1X, F8.3)"
                                  "      END")))
  ;; no reserved words: keywords can be variables
  (is-equal '(:assign :assign :assign) (let ((u (car (f77-parse (ftn-lines "      DO10I = 1" "      IF = 2" "      END = 3" "      END")))))
                                         (mapcar #'fn-kind (butlast (fu-stmts u)))))
  ;; a DO statement and an assignment to a variable named DO10I
  (is-equal :do (fn-kind (car (fu-stmts (car (f77-parse (ftn-lines "      DO 10 I = 1, 10" "      END")))))))
  (is-equal :assign (fn-kind (car (fu-stmts (car (f77-parse (ftn-lines "      DO 10 I = 1.10" "      END"))))))))

(deftest fortran-parser-expressions
  (flet ((expr (text) (let ((*f7t* (coerce (f77-tokenize text) 'vector)) (*f7p* 0)) (%f7-parse-expr))))
    (is-equal '(:bin :+ (:int 1) (:bin :* (:int 2) (:int 3))) (expr "1+2*3"))
    (is-equal '(:bin :** (:int 2) (:bin :** (:int 3) (:int 2))) (expr "2**3**2"))
    (is-equal '(:un :- (:bin :** (:name "A") (:int 2))) (expr "-A**2"))
    (is-equal '(:bin :// (:bin :// (:name "A") (:name "B")) (:name "C")) (expr "A//B//C"))
    (is-equal '(:ref "A" ((:name "I") (:int 2))) (expr "A(I,2)"))
    (is-equal '(:ref "S" ((:slice (:int 1) (:int 3)))) (expr "S(1:3)"))
    (is-equal '(:bin :or (:bin :and (:name "A") (:name "B")) (:name "C")) (expr "A.AND.B.OR.C"))
    (is-equal '(:complex (:real 1.0) (:real 2.0)) (expr "(1.0,2.0)"))))

(deftest fortran-parser-errors
  (multiple-value-bind (units errors) (f77-parse (ftn-lines "      PROGRAM T"
                                                           "      X = (1 + "
                                                           "      Y = 2"
                                                           "      FOOBAR"
                                                           "      END"))
    (is-equal 1 (length units))
    (is-equal '(2 4) (mapcar #'f77-error-line errors))
    (is (ftn-has (f77-error-message (cadr errors)) "Unrecognized statement")))
  ;; text beyond column 72 is ignored, and the error says so
  (is (ftn-has (f77-error-message (car (nth-value 1 (f77-parse (format nil "      PRINT *, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', NOT(X, 1)~%      END~%")))))
               "column 72"))
  ;; a unit without END
  (is (ftn-has (f77-error-message (car (last (nth-value 1 (f77-parse (ftn-lines "      X = 1")))))) "Missing END"))
  ;; INCLUDE
  (let ((inc (ftn-tmp "inc.f")) (src (ftn-tmp "main.f")))
    (with-open-file (s inc :direction :output) (write-string (ftn-lines "      Y = 2" "      Z = 3") s))
    (with-open-file (s src :direction :output) (write-string (ftn-lines "      X = 1" (format nil "      INCLUDE '~A'" inc) "      END") s))
    (is-equal '(:assign :assign :assign :end) (mapcar #'fn-kind (fu-stmts (car (f77-parse (%read-file-string src) :file src)))))
    (delete-file inc) (delete-file src)))

;;; ---- formatted input and output ------------------------------------------------------------------------------------

(deftest fortran-edit-descriptors
  (is-equal "   3.142" (%f7-edit-f 3.14159d0 8 3 0 nil))
  (is-equal "-0.50" (%f7-edit-f -0.5d0 5 2 0 nil))
  (is-equal ".50" (%f7-edit-f 0.5d0 3 2 0 nil))
  (is-equal "*****" (%f7-edit-f 12345.678d0 5 2 0 nil))
  (is-equal "  3." (%f7-edit-f 2.5d0 4 0 0 nil))
  (is-equal "+1.50" (%f7-edit-f 1.5d0 5 2 0 t))
  (is-equal "  0.1235E+05" (%f7-edit-e 12345.678d0 12 4 nil 0 nil #\E))
  (is-equal "  1.2346E+04" (%f7-edit-e 12345.678d0 12 4 nil 1 nil #\E))
  (is-equal "  0.1235D+05" (%f7-edit-e 12345.678d0 12 4 nil 0 nil #\D))
  (is-equal "0.1235E+005" (%f7-edit-e 12345.678d0 11 4 3 0 nil #\E))
  (is-equal "   12345.7    " (%f7-edit-g 12345.678d0 14 6 nil 0 nil))
  (is-equal "  0.100000E+11" (%f7-edit-g 1d10 14 6 nil 0 nil))
  (is-equal "   42" (%f7-edit-i 42 5 nil nil))
  (is-equal "***" (%f7-edit-i -1234 3 nil nil))
  (is-equal "  007" (%f7-edit-i 7 5 3 nil))
  (is-equal "42" (%f7-edit-i 42 0 nil nil))
  (is-equal "   FF" (%f7-edit-radix 255 5 nil 16))
  (is-equal "  101" (%f7-edit-radix 5 5 nil 2))
  (is-equal "0.1" (%f7-list-real 0.1d0 nil))
  (is-equal "0.1" (%f7-list-real (%f7-single 0.1d0) t))
  (is-equal "3.1415927" (%f7-list-real (%f7-single 3.141592653589793d0) t))
  (is-equal "3.141592653589793" (%f7-list-real 3.141592653589793d0 nil))
  (is-equal "1.0D-05" (%f7-list-real 1d-5 nil))
  (is-equal "100.0" (%f7-list-real 100d0 t))
  (is-equal "0.0" (%f7-list-real 0d0 t))
  ;; a double that needs all seventeen digits (the shortest-digits search must not give up)
  (is-equal "250275025.00000778" (%f7-list-real 2.5027502500000778d8 nil))
  (is-equal (format nil " 250275025.00000778~%")
            (ftn-run (ftn-lines "      DOUBLE PRECISION S" "      S = 250275025.00000778D0" "      PRINT *, S" "      END"))))

(deftest fortran-format-parsing
  (is-equal '((:x 1) (:str "X=") (:f 1 8 3 nil) (:i 3 5 nil) (:slash 1) (:group 2 ((:a 1 nil) (:i 1 3 nil))) (:e 1 12 4 2) (:p 2) (:f 1 6 2 nil))
            (f77-parse-format "(1X, 'X=', F8.3, 3I5, /, 2(A, I3), E12.4E2, 2PF6.2)"))
  (is-equal '((:i 1 5 nil) (:slash 3) (:colon) (:tl 2) (:tr 4) (:t 10) (:blank :zero) (:s :plus))
            (f77-parse-format "(i5,3/,:,tl2,tr4,t10,bz,sp)"))
  (is-equal '((:str "Hello") (:str "it's")) (f77-parse-format "(5HHello,'it''s')"))
  (signals-error (f77-parse-format "(I5"))
  (signals-error (f77-parse-format "(Q5)"))
  (signals-error (f77-parse-format "I5")))

(defun ftn-write (fmt &rest items)
  (let ((out (make-string-output-stream)))
    (let ((*f77-env* (%make-fenv :output out)))
      (let ((st (%f7-begin :write 6 (if (eq fmt :list) nil (f77-parse-format fmt)))))
        (dolist (it items) (%f7-put st (car it) (cdr it)))
        (%f7-end st)))
    (get-output-stream-string out)))

(defun ftn-read (text fmt &rest kinds)
  (let ((*f77-env* (%make-fenv :input (make-string-input-stream text))) (vals nil))
    (let ((st (%f7-begin :read 5 (if (eq fmt :list) nil (f77-parse-format fmt)))))
      (dolist (k kinds) (%f7-get st (car k) (lambda (v) (push v vals)) (cdr k)))
      (%f7-end st))
    (nreverse vals)))

(deftest fortran-format-output
  (let ((nl (string #\Newline)))
    (is-equal (format nil "    42   3.142~%") (ftn-write "(1X,I5,F8.3)" '(42 . :integer) '(3.14159d0 . :real)))
    ;; reversion: the format is used again, on a new record
    (is-equal (format nil "  1~%  2~%  3~%") (ftn-write "(I3)" '(1 . :integer) '(2 . :integer) '(3 . :integer)))
    ;; reversion restarts at the last group
    (is-equal (format nil "AB 1 2~% 3~%") (ftn-write "('AB',(I2,I2))" '(1 . :integer) '(2 . :integer) '(3 . :integer)))
    (is-equal (format nil " X= 1~% Y= 2~%") (ftn-write "(' X=',I2/' Y=',I2)" '(1 . :integer) '(2 . :integer)))
    (is-equal (format nil " 1, 2~%") (ftn-write "(3(I2,:,','))" '(1 . :integer) '(2 . :integer)))
    (is-equal (format nil "hablo~%") (ftn-write "(A5,T2,'ab')" '("hello" . :character)))
    (is-equal (format nil "ab   ~%") (ftn-write "(A5)" '("ab   " . :character)))
    (is-equal (format nil "   ab~%") (ftn-write "(A5)" '("ab" . :character)))
    (is-equal (format nil "abc~%") (ftn-write "(A3)" '("abcdef" . :character)))
    (is-equal (format nil "  T F~%") (ftn-write "(L3,L2)" '(t . :logical) '(nil . :logical)))
    (is-equal (format nil "a  b~%") (ftn-write "('a',2X,'b')"))
    (is-equal nl (ftn-write "()"))
    ;; list-directed output
    (is-equal (format nil " 42 3.5 str T (1.0,2.0)~%")
              (ftn-write :list '(42 . :integer) '(3.5d0 . :real) '("str" . :character) '(t . :logical) '(#C(1d0 2d0) . :complex)))
    ;; a data item whose descriptor has the wrong type is an error
    (signals-error (ftn-write "(I5)" '(1.5d0 . :real)))
    (signals-error (ftn-write "(F5.1)" '("x" . :character)))
    (signals-error (ftn-write "('x')" '(1 . :integer)))))

(deftest fortran-format-input
  (is-equal '(12 3.5d0) (ftn-read "  12 3.5" "(I4,F4.1)" '(:integer) '(:double)))
  (is-equal '(1.5d0) (ftn-read "15" "(F2.1)" '(:double)))
  (is-equal '(1500d0) (ftn-read " 1.5E3" "(F6.0)" '(:double)))
  (is-equal '(123) (ftn-read "1 2 3" "(I5)" '(:integer)))
  (is-equal '(10203) (ftn-read "1 2 3" "(BZ,I5)" '(:integer)))
  (is-equal '("abc  " t) (ftn-read "abc  T" "(A5,L1)" '(:character . 5) '(:logical)))
  (is-equal '("de") (ftn-read "abcde" "(3X,A2)" '(:character . 2)))
  ;; list-directed
  (is-equal '(1 2.5d0 "abc  " t) (ftn-read "1 2.5 abc T" :list '(:integer) '(:double) '(:character . 5) '(:logical)))
  (is-equal '(1 3) (ftn-read "1,,3" :list '(:integer) '(:integer) '(:integer)))
  (is-equal '(1 2 3 4) (ftn-read (format nil "1 2~%3 4") :list '(:integer) '(:integer) '(:integer) '(:integer)))
  (is-equal '(7 7 7 "x y") (ftn-read "3*7 'x y'" :list '(:integer) '(:integer) '(:integer) '(:character . 3)))
  (is-equal '(#C(1.0 2.0)) (ftn-read "(1.0,2.0)" :list '(:complex)))
  (is-equal '(1 2) (ftn-read "1 2 / 3" :list '(:integer) '(:integer) '(:integer)))
  (is-equal '(1.5d0) (ftn-read "1.5D0" :list '(:double)))
  ;; end of file and bad data are I/O errors with codes
  (is-equal -1 (handler-case (ftn-read "1 2" :list '(:integer) '(:integer) '(:integer)) (f77-io-error (e) (f77-error-code e))))
  (signals-error (ftn-read "abc" "(I3)" '(:integer)))
  (signals-error (ftn-read "1.5" :list '(:integer))))

;;; ---- the language -----------------------------------------------------------------------------------------------

(deftest fortran-hello-and-arithmetic
  (is-equal (format nil " Hello, world~%") (ftn-run (ftn-lines "      PROGRAM HELLO" "      PRINT *, 'Hello, world'" "      END")))
  (is-equal (format nil " 1024 8.0 3 -3 -1 2.5~%")
            (ftn-run (ftn-lines "      PRINT *, 2**10, 2.0**3, 10 / 3, -7 / 2, MOD(-7, 2), 5 / 2.0" "      END")))
  ;; integers have no size limit
  (is-equal (format nil " 265252859812191058636308480000000~%")
            (ftn-run (ftn-lines "      INTEGER N, F" "      F = 1" "      DO 5 N = 1, 30" "      F = F * N" "    5 CONTINUE" "      PRINT *, F" "      END")))
  (is-equal (format nil " 1267650600228229401496703205376~%") (ftn-run (ftn-lines "      PRINT *, 2**100" "      END")))
  ;; REAL has single precision and DOUBLE PRECISION double
  (is-equal (format nil " 0.1 0.1~%")
            (ftn-run (ftn-lines "      REAL X" "      DOUBLE PRECISION D" "      X = 0.1" "      D = 0.1D0" "      PRINT *, X, D" "      END")))
  (is-equal (format nil " 0.10000000149011612~%") (ftn-run (ftn-lines "      DOUBLE PRECISION D" "      D = 0.1" "      PRINT *, D" "      END")))
  ;; implicit typing: I-N integer, the rest real
  (is-equal (format nil " 2 2.5~%") (ftn-run (ftn-lines "      N = 5 / 2" "      X = 5.0 / 2" "      PRINT *, N, X" "      END")))
  (is-equal (format nil " 3 3.0~%") (ftn-run (ftn-lines "      N = 3.7" "      X = N" "      PRINT *, N, X" "      END"))))

(deftest fortran-control-flow
  ;; DO loops: step, zero trips, real variable, the variable's final value
  (is-equal (format nil " 12 13~%") (ftn-run (ftn-lines "      N = 0" "      DO 1 I = 10, 1, -3" "      N = N + 1" "    1 CONTINUE" "      DO 2 I = 1, 10, 4" "    2 N = N + 1" "      PRINT *, N + 5, I" "      END")))
  (is-equal (format nil " 0 5~%") (ftn-run (ftn-lines "      N = 0" "      DO 1 I = 5, 1" "      N = N + 1" "    1 CONTINUE" "      PRINT *, N, I" "      END")))
  (is-equal (format nil " 0.1 0.2~% 0.2 0.4~% 0.3 0.6~% 0.4 0.8~%")
            (ftn-run (ftn-lines "      REAL X" "      DO 1 X = 0.1, 0.45, 0.1" "      PRINT *, X, X * 2" "    1 CONTINUE" "      END")))
  ;; nested DO loops sharing one terminal statement
  (is-equal (format nil " 36~%") (ftn-run (ftn-lines "      M = 0" "      DO 10 I = 1, 3" "      DO 10 J = 1, 3" "   10 M = M + I*J" "      PRINT *, M" "      END")))
  ;; ENDDO and DO WHILE
  (is-equal (format nil " 6~%") (ftn-run (ftn-lines "      N = 0" "      DO I = 1, 3" "        N = N + I" "      ENDDO" "      PRINT *, N" "      END")))
  (is-equal (format nil " 4~%") (ftn-run (ftn-lines "      N = 1" "      DO WHILE (N .LT. 4)" "        N = N + 1" "      END DO" "      PRINT *, N" "      END")))
  ;; a labeled ENDDO: GO TO its label continues the loop
  (is-equal (format nil " 2~%") (ftn-run (ftn-lines "      N = 0" "      DO 10 I = 1, 4" "        IF (MOD(I, 2) .EQ. 0) GO TO 10" "        N = N + 1" "   10 ENDDO" "      PRINT *, N" "      END")))
  ;; block IF with ELSE IF and ELSE
  (let ((prog (ftn-lines "      READ *, N" "      IF (N .LT. 0) THEN" "        PRINT *, 'neg'" "      ELSE IF (N .EQ. 0) THEN" "        PRINT *, 'zero'" "      ELSE" "        PRINT *, 'pos'" "      END IF" "      END")))
    (is-equal (format nil " neg~%") (ftn-run prog :input (make-string-input-stream "-3")))
    (is-equal (format nil " zero~%") (ftn-run prog :input (make-string-input-stream "0")))
    (is-equal (format nil " pos~%") (ftn-run prog :input (make-string-input-stream "7"))))
  ;; computed GO TO, assigned GO TO, arithmetic IF, logical IF
  (let ((out (ftn-run (ftn-lines "      CHARACTER*12 S" "      I = 2"
                                 "      GO TO (10, 20, 30), I" "   10 S = 'ten'" "      GO TO 40" "   20 S = 'twenty'" "      GO TO 40" "   30 S = 'thirty'"
                                 "   40 PRINT *, S" "      ASSIGN 50 TO J" "      GO TO J" "   45 PRINT *, 'no'" "   50 PRINT *, 'assigned'"
                                 "      X = -1.5" "      IF (X) 60, 70, 80" "   60 PRINT *, 'neg'" "      GO TO 90" "   70 PRINT *, 'zero'" "      GO TO 90"
                                 "   80 PRINT *, 'pos'" "   90 IF (X .LT. 0) PRINT *, 'logical'" "      IF (X .GT. 0) PRINT *, 'not shown'" "      END"))))
    (is-equal (format nil " ~12A~% assigned~% neg~% logical~%" "twenty") out))
  ;; STOP with a message ends the program
  (is-equal (format nil " before~%STOP done~%") (ftn-run (ftn-lines "      PRINT *, 'before'" "      STOP 'done'" "      PRINT *, 'after'" "      END")))
  (is-equal (format nil "") (ftn-run (ftn-lines "      STOP" "      PRINT *, 'after'" "      END"))))

(deftest fortran-routines
  (is-equal (format nil " 7.5 4.5~%")
            (ftn-run (ftn-lines "      PROGRAM T2" "      CALL SUB(3, X)" "      PRINT *, X, FN(4)" "      END"
                                "      SUBROUTINE SUB(N, Y)" "      INTEGER N" "      Y = N * 2.5" "      RETURN" "      END"
                                "      FUNCTION FN(K)" "      INTEGER K" "      FN = K + 0.5" "      END")))
  ;; arguments are passed by reference
  (is-equal (format nil " 11 12~%")
            (ftn-run (ftn-lines "      INTEGER A(2)" "      A(1) = 5" "      A(2) = 6" "      CALL ADD(A, 2, 6)" "      PRINT *, A" "      END"
                                "      SUBROUTINE ADD(V, N, K)" "      INTEGER V(N)" "      DO 1 I = 1, N" "    1 V(I) = V(I) + K" "      END")))
  ;; a constant argument is a temporary
  (is-equal (format nil " 3 1~%")
            (ftn-run (ftn-lines "      I = 1" "      CALL INC(I, 2)" "      CALL INC(1, 5)" "      J = 1" "      PRINT *, I, J" "      END"
                                "      SUBROUTINE INC(K, N)" "      K = K + N" "      END")))
  ;; array sections: an element is a section of the array; adjustable and assumed-size dummies
  (is-equal (format nil " 11.0 21.0~% 12.0 22.0~% 13.0 23.0~% 21.0 22.0 23.0~%")
            (ftn-run (ftn-lines "      REAL A(3,2)" "      DO 1 J = 1, 2" "      DO 1 I = 1, 3" "    1 A(I,J) = I + 10*J"
                                "      CALL PR(A, 3, 2)" "      CALL PR1(A(1,2), 3)" "      END"
                                "      SUBROUTINE PR(X, N, M)" "      REAL X(N, M)" "      DO 5 I = 1, N" "    5 PRINT *, (X(I,J), J=1,M)" "      END"
                                "      SUBROUTINE PR1(V, N)" "      DIMENSION V(*)" "      PRINT *, (V(I), I=1,N)" "      END")))
  ;; alternate returns
  (is-equal (format nil " first~% second2~%")
            (ftn-run (ftn-lines "      CALL CHK(5, *100, *200)" "      PRINT *, 'normal'" "      GO TO 300" "  100 PRINT *, 'first'" "      GO TO 300" "  200 PRINT *, 'second'"
                                "  300 CONTINUE" "      CALL CHK(-5, *111, *222)" "      PRINT *, 'normal2'" "      GO TO 333" "  111 PRINT *, 'first2'" "      GO TO 333"
                                "  222 PRINT *, 'second2'" "  333 CONTINUE" "      END"
                                "      SUBROUTINE CHK(N, *, *)" "      IF (N .GT. 0) RETURN 1" "      IF (N .LT. 0) RETURN 2" "      RETURN" "      END")))
  ;; statement functions, and a procedure as an argument
  (is-equal (format nil " 10.0 100.0~%")
            (ftn-run (ftn-lines "      EXTERNAL SQ" "      F(X) = X*X + 1.0" "      PRINT *, F(3.0), APPLY(SQ, 10.0)" "      END"
                                "      FUNCTION SQ(X)" "      SQ = X*X" "      END"
                                "      FUNCTION APPLY(F, X)" "      EXTERNAL F" "      APPLY = F(X)" "      END")))
  ;; an intrinsic function passed as an argument
  (is-equal (format nil " 3.0~%") (ftn-run (ftn-lines "      INTRINSIC SQRT" "      PRINT *, APPLY(SQRT, 9.0)" "      END" "      FUNCTION APPLY(F, X)" "      EXTERNAL F" "      APPLY = F(X)" "      END"))))

(deftest fortran-storage
  ;; COMMON blocks, EQUIVALENCE, DATA with repeat counts and implied DO
  (is-equal (format nil " 1.5 2.0 2.0 2.0~% 9.0~% 7~%")
            (ftn-run (ftn-lines "      PROGRAM T1" "      COMMON /BLK/ A, B(3)" "      INTEGER K(4)" "      REAL R(4)" "      EQUIVALENCE (K(1), R(1))"
                                "      DATA A /1.5/, B /3*2.0/" "      CALL SHOW" "      K(1) = 7" "      PRINT *, K(1)" "      END"
                                "      SUBROUTINE SHOW" "      COMMON /BLK/ X, Y(3)" "      PRINT *, X, Y" "      Y(2) = 9.0" "      PRINT *, Y(2)" "      END")))
  (is-equal (format nil " 1 4 9 16~%")
            (ftn-run (ftn-lines "      INTEGER A(4)" "      DATA (A(I), I=1,4) /1, 4, 9, 16/" "      PRINT *, A" "      END")))
  ;; EQUIVALENCE overlays: the second element of A is the first of B
  (is-equal (format nil " 7~%") (ftn-run (ftn-lines "      INTEGER A(3), B(2)" "      EQUIVALENCE (A(2), B(1))" "      A(2) = 7" "      PRINT *, B(1)" "      END")))
  ;; BLOCK DATA initialises COMMON
  (is-equal (format nil " 42 2.5~%")
            (ftn-run (ftn-lines "      BLOCK DATA INIT" "      COMMON /CB/ N, X" "      DATA N /42/, X /2.5/" "      END"
                                "      PROGRAM T" "      COMMON /CB/ N, X" "      PRINT *, N, X" "      END")))
  ;; DOUBLE PRECISION and COMPLEX take two entries, so these two are different variables
  (is-equal (format nil " 3.0 4.0~%")
            (ftn-run (ftn-lines "      DOUBLE PRECISION D(2)" "      REAL R(4)" "      EQUIVALENCE (D(1), R(1))" "      R(3) = 3.0" "      R(4) = 4.0" "      PRINT *, R(3), R(4)" "      END")))
  ;; PARAMETER
  (is-equal (format nil " 10 6.283185~%")
            (ftn-run (ftn-lines "      PARAMETER (N = 5*2, TWOPI = 6.283185)" "      INTEGER A(N)" "      PRINT *, N, TWOPI" "      END")) ))

(deftest fortran-characters
  (is-equal (format nil " HELLO      | HELLO      |~%       5 0~% JELLO     ~% LESS~% 65 B~%   abcd~%")
            (ftn-run (ftn-lines "      CHARACTER*10 S, T" "      CHARACTER*5 U" "      S = 'HELLO'" "      T = S // ' WORLD'" "      PRINT *, S, '|', T, '|'"
                                "      U = T(7:)" "      PRINT *, U, LEN(U), INDEX(T, 'WOR')" "      S(1:1) = 'J'" "      PRINT *, S"
                                "      IF (S .LT. 'K') PRINT *, 'LESS'" "      PRINT *, ICHAR('A'), CHAR(66)" "      WRITE (*, '(A5,A)') 'ab', 'cd'" "      END")))
  ;; comparison pads with blanks; assignment truncates
  (is-equal (format nil " T T abc~%") (ftn-run (ftn-lines "      CHARACTER*3 S" "      S = 'abcdef'" "      PRINT *, 'ab' .EQ. 'ab   ', LLT('a', 'b'), S" "      END")))
  ;; character arrays and functions of characters
  (is-equal (format nil " c a~%")
            (ftn-run (ftn-lines "      CHARACTER*1 C(3)" "      C(1) = 'a'" "      C(2) = 'b'" "      C(3) = 'c'" "      PRINT *, C(3), C(1)" "      END")))
  (is-equal (format nil " HI~%") (ftn-run (ftn-lines "      CHARACTER*2 GREET" "      PRINT *, GREET()" "      END" "      CHARACTER*2 FUNCTION GREET()" "      GREET = 'hi'" "      GREET(1:2) = 'HI'" "      END"))))

(deftest fortran-complex-and-intrinsics
  (is-equal (format nil " (-3.0,4.0) -3.0 4.0 2.236068~%")
            (ftn-run (ftn-lines "      COMPLEX Z, W" "      Z = (1.0, 2.0)" "      W = Z * Z" "      PRINT *, W, REAL(W), AIMAG(W), CABS(Z)" "      END")))
  (is-equal (format nil " 0.3333333333333333 1.4142135623730951 1.4142135~%")
            (ftn-run (ftn-lines "      DOUBLE PRECISION D" "      D = 1.0D0 / 3.0D0" "      PRINT *, D, SQRT(2.0D0), SQRT(2.0)" "      END")))
  (is-equal (format nil " 3 -3 4 -4~% 3.0 4.0~%")
            (ftn-run (ftn-lines "      PRINT *, INT(3.9), INT(-3.9), NINT(3.5), NINT(-3.5)" "      PRINT *, AINT(3.9), ANINT(3.5)" "      END")))
  (is-equal (format nil " 3.0 5 1.0~% -2.0 1~%")
            (ftn-run (ftn-lines "      PRINT *, ABS(-3.0), MAX(1, 5, 3), MIN(2.0, 1.0)" "      PRINT *, SIGN(2.0, -1.0), DIM(3, 2)" "      END")))
  (is-equal (format nil " 0.0 1.0 1.0~%") (ftn-run (ftn-lines "      PRINT *, SIN(0.0), COS(0.0), EXP(0.0)" "      END")))
  (is-equal (format nil " 0.0 2.0~%") (ftn-run (ftn-lines "      PRINT *, LOG(1.0), LOG10(100.0)" "      END")))
  (is-equal (format nil " 0.7853982~%") (ftn-run (ftn-lines "      PRINT *, ATAN2(1.0, 1.0)" "      END"))))

(deftest fortran-bit-operations
  ;; the Department of Defense bit functions, binary patterns, and the logical operators on integers
  (is-equal (format nil " 240 241 15~% 1180591620717411303424 -1~%")
            (ftn-run (ftn-lines "      INTEGER I" "      I = Z'F0'" "      PRINT *, IAND(I, 255), IOR(I, 1), IEOR(I, 255)" "      PRINT *, ISHFT(1, 70), NOT(0)" "      END"))
            )
  (is-equal (format nil " F T F F T~%")
            (ftn-run (ftn-lines "      LOGICAL P, Q" "      P = .TRUE." "      Q = .FALSE." "      PRINT *, P .AND. Q, P .OR. Q, .NOT. P, P .EQV. Q, P .NEQV. Q" "      END")))
  (is-equal (format nil " 1 T 7 5~%") (ftn-run (ftn-lines "      PRINT *, 5 .AND. 3, BTEST(5, 0), IBITS(255, 2, 3), ISHFT(20, -2)" "      END")))
  (is-equal (format nil "      FF  10   1010A~%") (ftn-run (ftn-lines "      WRITE (*, '(Z8, O4, B6, Z2.2)') 255, 8, 5, 10" "      END")))
  (is-equal (format nil " 4 14 5~%") (ftn-run (ftn-lines "      INTEGER I" "      I = B'1100'" "      CALL MVBITS(2, 0, 2, I, 0)" "      PRINT *, IBCLR(5, 0) , I + 0, IBSET(4, 0)" "      END")))
  (is-equal (format nil " 8 6~%") (ftn-run (ftn-lines "      PRINT *, ISHFTC(1, 3, 4), ISHFTC(3, 1, 3)" "      END"))))

(deftest fortran-integer-logical-coercion
  (is-equal (format nil " T 1 -2~% T~%")
            (ftn-run (ftn-lines "      LOGICAL L" "      INTEGER I" "      I = 5" "      L = I" "      PRINT *, L, L + 0, .NOT. L + 0" "      L = 0" "      PRINT *, .NOT. L" "      END")) ))

(deftest fortran-uninitialized-values
  ;; reading a variable that was never given a value is an error that names it
  (is (ftn-has (ftn-error (ftn-lines "      INTEGER N" "      PRINT *, N" "      END")) "variable n is used before it has been given a value"))
  (is (ftn-has (ftn-error (ftn-lines "      REAL A(3)" "      A(1) = 1.0" "      PRINT *, A(1) + A(2)" "      END")) "variable a is used"))
  ;; Init to Zero starts storage at zero, with the right type
  (is-equal (format nil " 0 0.0 F~%") (ftn-run (ftn-lines "      INTEGER N" "      REAL X" "      LOGICAL L" "      PRINT *, N, X, L" "      END") :init-to-zero t))
  (is-equal (format nil " 0.0~%") (ftn-run (ftn-lines "      REAL A(3)" "      PRINT *, A(2)" "      END") :init-to-zero t))
  ;; variables in COMMON are checked too
  (is (ftn-has (ftn-error (ftn-lines "      COMMON /B/ X" "      PRINT *, X" "      END")) "variable x is used"))
  ;; a function that never gives its value a result
  (is (ftn-has (ftn-error (ftn-lines "      PRINT *, F(1)" "      END" "      FUNCTION F(X)" "      END")) "returned without giving its value a result")))

(deftest fortran-run-time-errors
  (is (ftn-has (ftn-error (ftn-lines "      INTEGER A(3)" "      A(4) = 1" "      END")) "subscript 4 of a is outside the array bounds 1:3"))
  (is (ftn-has (ftn-error (ftn-lines "      INTEGER A(3)" "      A(4) = 1" "      END")) "(in FORTRAN routine .main., line 2)"))
  (is (ftn-has (ftn-error (ftn-lines "      INTEGER I, J" "      I = 0" "      J = 5 / I" "      END")) "Integer division by zero"))
  (is (ftn-has (ftn-error (ftn-lines "      CHARACTER*3 S" "      S = 'abc'" "      PRINT *, S(2:5)" "      END")) "substring 2:5"))
  (is (ftn-has (ftn-error (ftn-lines "      X = SQRT(-1.0)" "      END")) "SQRT of the negative"))
  (is (ftn-has (ftn-error (ftn-lines "      CALL NOSUCH(1)" "      END")) "FORTRAN routine nosuch is not defined"))
  (is (ftn-has (ftn-error (ftn-lines "      CALL S(1)" "      END" "      SUBROUTINE S(A, B)" "      END")) "Wrong number of arguments"))
  ;; the error names the routine and line even inside a called routine
  (is (ftn-has (ftn-error (ftn-lines "      CALL S" "      END" "      SUBROUTINE S" "      INTEGER A(2)" "      A(0) = 1" "      END")) "(in FORTRAN routine s, line 5)")))

(deftest fortran-type-checks
  (is-equal '("A CHARACTER value cannot be assigned to an INTEGER variable" "An INTEGER value cannot be assigned to a CHARACTER variable")
            (ftn-compile-errors (ftn-lines "      CHARACTER*5 S" "      INTEGER I" "      I = S" "      S = I + 1" "      END")))
  (is-equal '("The argument of SQRT must be a number, not CHARACTER"
              "The specific function IABS needs an argument of type INTEGER, not REAL"
              "The statement label 99 is not defined in this program unit")
            (ftn-compile-errors (ftn-lines "      X = SQRT('A')" "      Y = IABS(2.5)" "      GO TO 99" "      END")))
  (is-equal '("The name X is not declared (the unit says IMPLICIT NONE)") (ftn-compile-errors (ftn-lines "      IMPLICIT NONE" "      X = 1" "      END")))
  (is-equal '("The array A has 1 dimension but is given 2 subscripts") (ftn-compile-errors (ftn-lines "      INTEGER A(3)" "      A(1,2) = 1" "      END")))
  (is-equal '("The array V is used without subscripts") (ftn-compile-errors (ftn-lines "      INTEGER V(3)" "      I = V + 1" "      END")))
  (is-equal '("The named constant K cannot be assigned to") (ftn-compile-errors (ftn-lines "      PARAMETER (K = 1)" "      K = 2" "      END")))
  (is-equal '("The block IF has no END IF") (ftn-compile-errors (ftn-lines "      IF (.TRUE.) THEN" "      X = 1" "      END")))
  (is-equal '("The terminal statement 10 of the DO loop was not found") (ftn-compile-errors (ftn-lines "      DO 10 I = 1, 3" "      X = 1" "      END")))
  (is-equal '("The statement label 5 is defined twice") (ftn-compile-errors (ftn-lines "    5 X = 1" "    5 Y = 2" "      END")))
  (is-equal '("The type of X is declared twice") (ftn-compile-errors (ftn-lines "      REAL X" "      INTEGER X" "      END")))
  (is-equal '("Cannot compare CHARACTER with INTEGER") (ftn-compile-errors (ftn-lines "      CHARACTER*2 S" "      IF (S .EQ. 1) X = 1" "      END")))
  ;; calls are checked against routines compiled before: argument count, argument types, function or subroutine
  (f77-compile-string (ftn-lines "      SUBROUTINE CHKA(A, B)" "      INTEGER A" "      REAL B" "      END" "      FUNCTION CHKF(X)" "      CHKF = X" "      END"))
  (is-equal '("chka takes 2 arguments but is called with 1") (ftn-compile-errors (ftn-lines "      CALL CHKA(1)" "      END")))
  (is-equal '("Argument 1 of chka is REAL but the routine declares it INTEGER") (ftn-compile-errors (ftn-lines "      CALL CHKA(1.5, 2.0)" "      END")))
  (is-equal '("CHKF is a function, not a subroutine") (ftn-compile-errors (ftn-lines "      CALL CHKF(1.0)" "      END")))
  (is-equal '("The function CHKF is declared INTEGER here but is defined as REAL") (ftn-compile-errors (ftn-lines "      INTEGER CHKF" "      I = CHKF(1.0)" "      END")))
  ;; a good program after bad ones compiles, and every error is reported with its line
  (is-equal '(3 5) (mapcar #'f77-error-line (nth-value 1 (f77-compile-string (ftn-lines "      INTEGER I" "      I = 1" "      I = 'x'" "      I = 2" "      I = 'y'" "      END"))))))

(deftest fortran-input-output
  ;; internal files, files, END=, IOSTAT=, REC=
  (is-equal (format nil "  12  34             ~% 46~%")
            (ftn-run (ftn-lines "      CHARACTER*20 BUF" "      INTEGER N, M" "      WRITE (BUF, '(I3,1X,I3)') 12, 34" "      PRINT *, BUF" "      READ (BUF, *) N, M" "      PRINT *, N + M" "      END")))
  (let ((f (ftn-tmp "o")))
    (is-equal (format nil " val  99             ~% 3.5 x                   ~% eof~%")
              (ftn-run (ftn-lines "      CHARACTER*20 BUF" (format nil "      OPEN (UNIT=10, FILE='~A', STATUS='UNKNOWN')" f)
                                  "      WRITE (10, '(A,I4)') 'val', 99" "      WRITE (10, *) 3.5, 'x'" "      CLOSE (10)"
                                  (format nil "      OPEN (UNIT=11, FILE='~A', STATUS='OLD')" f)
                                  "      READ (11, '(A)') BUF" "      PRINT *, BUF" "      READ (11, *, END=77) X, BUF" "      PRINT *, X, BUF"
                                  "      READ (11, *, END=77) X" "   77 PRINT *, 'eof'" "      CLOSE (11)" "      END")))
    (is-equal (format nil "val  99~% 3.5 x~%") (%read-file-string f))
    (delete-file f))
  ;; IOSTAT=
  (is-equal (format nil " 0 -1~%") (ftn-run (ftn-lines "      READ (5, *, IOSTAT=I) N" "      J = I" "      READ (5, *, IOSTAT=I) N" "      PRINT *, J, I" "      END") :input (make-string-input-stream "5")))
  ;; direct access
  (let ((f (ftn-tmp "d")))
    (is-equal (format nil " 33 11~%")
              (ftn-run (ftn-lines (format nil "      OPEN (UNIT=4, FILE='~A', ACCESS='DIRECT'," f)
                                  "     +      FORM='FORMATTED')"
                                  "      WRITE (4, '(I5)', REC=3) 33" "      WRITE (4, '(I5)', REC=1) 11"
                                  "      READ (4, '(I5)', REC=3) J" "      READ (4, '(I5)', REC=1) K" "      PRINT *, J, K" "      END")))
    (delete-file f))
  ;; unformatted
  (let ((f (ftn-tmp "u")))
    (is-equal (format nil " 1 2.5 abc~% 7~%")
              (ftn-run (ftn-lines "      CHARACTER*3 S" (format nil "      OPEN (UNIT=3, FILE='~A', FORM='UNFORMATTED')" f)
                                  "      WRITE (3) 1, 2.5, 'abc'" "      WRITE (3) 7" "      REWIND 3" "      READ (3) I, X, S" "      PRINT *, I, X, S" "      READ (3) J" "      PRINT *, J" "      END")))
    (delete-file f))
  ;; INQUIRE, BACKSPACE, ENDFILE
  (let ((f (ftn-tmp "i")))
    (is-equal (format nil " F T T 2~%")
              (ftn-run (ftn-lines "      LOGICAL EX, OP, E2" "      INTEGER N" (format nil "      INQUIRE (FILE='~A', EXIST=EX)" f)
                                  (format nil "      OPEN (UNIT=2, FILE='~A')" f)
                                  "      WRITE (2, *) 1" "      WRITE (2, *) 2"
                                  "      INQUIRE (UNIT=2, OPENED=OP, NUMBER=N)" (format nil "      INQUIRE (FILE='~A', OPENED=E2)" f)
                                  "      PRINT *, EX, OP, E2, N" "      CLOSE (2, STATUS='DELETE')" "      END")))
    (is (not (probe-file f))))
  ;; implicit file names and scratch files
  (is-equal (format nil " 5~%") (ftn-run (ftn-lines "      OPEN (UNIT=7, STATUS='SCRATCH')" "      WRITE (7, *) 5" "      REWIND 7" "      READ (7, *) N" "      PRINT *, N" "      END")))
  ;; reading the standard input
  (is-equal (format nil " 42 3.5~%")
            (ftn-run (ftn-lines "      READ *, N, X" "      PRINT *, N * 2, X + 1.0" "      END") :input (make-string-input-stream (format nil "21 2.5~%")))))

;;; ---- the Tool Kit -----------------------------------------------------------------------------------------------

(deftest fortran-execute-options
  ;; Save Environment keeps storage from one run to the next; Reload discards it
  (f77-compile-string (ftn-lines "      PROGRAM CNT" "      INTEGER N" "      N = N + 1" "      PRINT *, N" "      END"))
  (flet ((run (&rest o) (let ((out (make-string-output-stream))) (apply #'f77-execute 'ftn-user::cnt :output out :init-to-zero t o) (get-output-stream-string out))))
    (is-equal (format nil " 1~%") (run :save-environment t :reload t))
    (is-equal (format nil " 2~%") (run :save-environment t))
    (is-equal (format nil " 3~%") (run :save-environment t))
    (is-equal (format nil " 1~%") (run :save-environment t :reload t))
    (is-equal (format nil " 1~%") (run)))
  ;; the F77 package
  (let ((out (make-string-output-stream)))
    (f77:execute 'ftn-user::cnt :output out :init-to-zero t)
    (f77:xqt 'ftn-user::cnt :output out :init-to-zero t)
    (is-equal (format nil " 1~%") (subseq (get-output-stream-string out) 0 3)))
  ;; Pathname Default, Additional Externals, Units, Streams
  (let ((dir (ftn-tmp "dir")))
    (is-equal (format nil " 9~%")
              (progn (f77-compile-string (ftn-lines "      PROGRAM PD" "      OPEN (UNIT=3, FILE='pd.dat', STATUS='REPLACE')" "      WRITE (3, *) 9" "      CLOSE (3)" "      OPEN (UNIT=3, FILE='pd.dat')" "      READ (3, *) N" "      PRINT *, N" "      END"))
                     (let ((out (make-string-output-stream)))
                       (%make-directory-for-test dir)
                       (f77-execute 'ftn-user::pd :output out :pathname-defaults (%string-concat dir "/"))
                       (get-output-stream-string out))))
    (is (probe-file (%string-concat dir "/pd.dat"))))
  (f77-compile-string (ftn-lines "      PROGRAM EXT" "      CALL LISPFN(7, R)" "      PRINT *, R" "      END"))
  (is-equal (format nil " 49~%")
            (let ((out (make-string-output-stream)))
              (f77-execute 'ftn-user::ext :output out
                           :additional-externals (list (cons "LISPFN" (lambda (a r) (setf (svref (car r) (cdr r)) (* (f77-ref-value a) (f77-ref-value a)))))))
              (get-output-stream-string out)))
  (signals-error (f77-execute 'ftn-user::cnt :units '(nosuchunit)))
  (signals-error (f77-execute 'ftn-user::nosuchprogram))
  (is-equal (format nil " seen~%")
            (progn (f77-compile-string (ftn-lines "      PROGRAM STR" "      READ (20, *) N" "      IF (N .EQ. 3) PRINT *, 'seen'" "      END"))
                   (let ((out (make-string-output-stream)))
                     (f77-execute 'ftn-user::str :output out :streams (list (cons 20 (make-string-input-stream "3"))))
                     (get-output-stream-string out)))))

(defun %make-directory-for-test (dir)
  (ensure-directories-exist (%string-concat dir "/dummy")))

(deftest fortran-packages-and-lisp-calls
  ;; routines live in the package of the source, FTN-USER by default, which has no superiors
  (is-equal "FTN-USER" (package-name (symbol-package (car (f77-compile-string (ftn-lines "      SUBROUTINE PKGTEST" "      END"))))))
  (is-equal nil (package-use-list (find-package "FTN-USER")))
  (is (not (eq (find-symbol "LIST" "FTN-USER") 'list)))
  (is-equal '(ftn-user::pkgtest) (f77-compile-string (ftn-lines "      SUBROUTINE PKGTEST" "      END")))
  (f77-compile-string (ftn-lines "      PROGRAM P1" "      PRINT *, 'one'" "      END") :package "FTN-OTHER")
  (is (gethash "FTN-OTHER:P1" *f77-programs*))
  ;; an unnamed main program is .MAIN., and a second one replaces it
  (is-equal (format nil " second~%")
            (progn (f77-compile-string (ftn-lines "      PRINT *, 'first'" "      END") :package "FTN-MAIN")
                   (f77-compile-string (ftn-lines "      PRINT *, 'second'" "      END") :package "FTN-MAIN")
                   (let ((out (make-string-output-stream))) (f77-execute (intern ".MAIN." "FTN-MAIN") :output out) (get-output-stream-string out))))
  ;; Lisp calls compiled FORTRAN with plain values and with references
  (f77-compile-string (ftn-lines "      INTEGER FUNCTION TWICE(K)" "      INTEGER K" "      TWICE = 2*K" "      END"
                                 "      SUBROUTINE ADDTO(X, Y)" "      X = X + Y" "      END"))
  (is-equal 42 (ftn-user::twice 21))
  (let ((r (f77:make-ref 1.5d0))) (ftn-user::addto r 2.0d0) (is-equal 3.5d0 (f77:ref-value r)))
  ;; Lisp storage regions are addressed by integer offset
  (f77-compile-string (ftn-lines "      SUBROUTINE FILL(V, N)" "      INTEGER V(N)" "      DO 1 I = 1, N" "    1 V(I) = I * I" "      END"))
  (f77:with-fortran-number-data (data 5)
    (ftn-user::fill data 5)
    (is-equal '(1 4 9 16 25) (loop for i from 0 below 5 collect (f77-ref-element data i))))
  (f77-compile-string (ftn-lines "      SUBROUTINE UPCASE(S)" "      CHARACTER*3 S(2)" "      S(2) = 'xyz'" "      END"))
  (f77:with-fortran-character-data (chars 2 :length 3 :initial "abc")
    (ftn-user::upcase chars)
    (is-equal '("abc" "xyz") (list (f77-ref-element chars 0) (f77-ref-element chars 3))))
  ;; Lisp symbols named like FORTRAN names do not clash
  (f77-compile-string (ftn-lines "      REAL FUNCTION LIST(X)" "      LIST = X + 1" "      END"))
  (is-equal 3.0d0 (ftn-user::list 2.0d0))
  (is (not (eq (symbol-function 'ftn-user::list) (symbol-function 'cl:list)))))

(deftest fortran-compile-and-load-files
  (let ((src (ftn-tmp "prog.f")))
    (with-open-file (s src :direction :output)
      (write-string (ftn-lines "C -*- Mode: FORTRAN; Package: FTN-FILE -*-" "      PROGRAM FILEPROG" "      PRINT *, 'from a file'" "      END"
                               "      SUBROUTINE FILESUB(X)" "      X = 3" "      END")
                    s))
    (is-equal '("FORTRAN" "FTN-FILE") (let ((a (f77-file-attributes (%read-file-string src)))) (list (getf a :mode) (getf a :package))))
    (is-equal 2 (length (f77-load-file src)))
    (is-equal (format nil " from a file~%") (let ((out (make-string-output-stream))) (f77-execute 'ftn-file::fileprog :output out) (get-output-stream-string out)))
    ;; compile to a compiled-code file and load it in a fresh program table
    (let ((fasl (f77-compile-file src)))
      (is (probe-file fasl))
      (remhash "FTN-FILE:FILEPROG" *f77-programs*)
      (signals-error (f77-execute 'ftn-file::fileprog))
      (load fasl)
      (is-equal (format nil " from a file~%") (let ((out (make-string-output-stream))) (f77-execute 'ftn-file::fileprog :output out) (get-output-stream-string out)))
      (delete-file fasl))
    (is (f77-source-file-p "x.for")) (is (f77-source-file-p "X.F77")) (is (f77-source-file-p "a.ftn")) (is (not (f77-source-file-p "a.pas")))
    (delete-file src))
  ;; a file with errors is not loaded and its warnings are kept
  (let ((src (ftn-tmp "bad.f")))
    (with-open-file (s src :direction :output) (write-string (ftn-lines "      PROGRAM BADFILE" "      I = 'x'" "      END") s))
    (signals-error (f77-load-file src))
    (is-equal 1 (length (f77-compiler-warnings src)))
    (is-equal 2 (fwarning-line (car (f77-compiler-warnings src))))
    (delete-file src)))

(deftest fortran-def-program-and-systems
  (let* ((dir (ftn-tmp "sys")) (d (%string-concat dir "/")))
    (%make-directory-for-test dir)
    (with-open-file (s (%string-concat d "lib.f") :direction :output)
      (write-string (ftn-lines "      FUNCTION CUBE(X)" "      CUBE = X*X*X" "      END") s))
    (with-open-file (s (%string-concat d "main.f") :direction :output)
      (write-string (ftn-lines "      PROGRAM SYSMAIN" "      PRINT *, CUBE(3.0)" "      END") s))
    (f77:def-library cubes (:default-pathname d :default-package "FTN-SYS") "lib")
    (f77:def-program sysprog (:default-pathname d :default-package "FTN-SYS" :libraries (cubes)) "main")
    (is (find-system 'cubes))
    (is-equal '("lib.f") (system-components (find-system 'cubes)))
    (is-equal '(cubes) (system-depends-on (find-system 'sysprog)))
    (load-system 'sysprog :verbose nil)
    (is-equal (format nil " 27.0~%") (let ((out (make-string-output-stream))) (f77-execute 'ftn-sys::sysmain :output out) (get-output-stream-string out)))
    ;; compile-system produces compiled files for FORTRAN components
    (compile-system 'sysprog :verbose nil :reload t)
    (is (probe-file (%string-concat d "lib.nfasl")))
    (is (probe-file (%string-concat d "main.nfasl")))))

(deftest fortran-warnings-database
  (f77-compile-string (ftn-lines "      X = 'a' + 1" "      END") :file "warn.f")
  (is-equal 1 (length (f77-compiler-warnings "warn.f")))
  (is (ftn-has (fwarning-message (car (f77-compiler-warnings "warn.f"))) "numeric operands"))
  (f77-compile-string (ftn-lines "      X = 1" "      END") :file "warn.f")
  (is-equal nil (f77-compiler-warnings "warn.f")))

(deftest fortran-execute-command-options
  (is-equal '(:init-to-zero t :reload nil) (let ((p (f77-parse-execute-options "Init to Zero yes, Reload no"))) (list :init-to-zero (getf p :init-to-zero) :reload (getf p :reload))))
  (is-equal t (getf (f77-parse-execute-options "init") :init-to-zero))
  (is-equal t (getf (f77-parse-execute-options "save env") :save-environment))
  (is-equal "/tmp/x/" (getf (f77-parse-execute-options "Pathname Default /tmp/x/") :pathname-defaults))
  (is-equal '(:numerics :stats) (getf (f77-parse-execute-options "Libraries (numerics stats)") :libraries))
  (is-equal :sysx (getf (f77-parse-execute-options "System sysx") :system))
  (is-equal '(100 50) (let ((p (f77-parse-execute-options "Extra Number Space 100, Extra Character Space 50")))
                        (list (getf p :extra-number-space) (getf p :extra-character-space))))
  (is-equal nil (f77-parse-execute-options ""))
  (signals-error (f77-parse-execute-options "Bogus option"))
  ;; the command runs the program with them
  (f77-compile-string (ftn-lines "      PROGRAM OPTPROG" "      INTEGER N" "      PRINT *, N" "      END"))
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines ":execute fortran optprog Init to Zero yes"))
    (is (pane-has lw " 0"))
    (is (not (pane-has lw "used before")))))

(deftest fortran-bit-substrings
  ;; I(first:last) is the field of bits first through last, bit 0 being the least significant; it can be assigned
  (is-equal (format nil " 176 11 0~% 181~%")
            (ftn-run (ftn-lines "      INTEGER I, J" "      I = 0" "      I(4:7) = 11" "      J = I(4:7)" "      PRINT *, I, J, I(0:3)"
                                "      I(0:3) = 5" "      PRINT *, I" "      END")))
  (is-equal (format nil " 5~%") (ftn-run (ftn-lines "      INTEGER I" "      I = 255" "      PRINT *, I(1:3) - 2" "      END")))
  (is (ftn-has (ftn-error (ftn-lines "      INTEGER I" "      I = 1" "      J = I(3:1)" "      END")) "bit substring 3:1"))
  (is-equal '("A bit substring needs both bounds, I(first:last)") (ftn-compile-errors (ftn-lines "      INTEGER I" "      I = 1" "      J = I(2:)" "      END"))))

(deftest fortran-listener-commands
  (f77-compile-string (ftn-lines "      PROGRAM CMDTEST" "      PRINT *, 'hello from FORTRAN'" "      END"))
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines ":execute fortran cmdtest" ":show fortran programs"))
    (is (pane-has lw "hello from FORTRAN"))
    (is (pane-has lw "FTN-USER:cmdtest")))
  (let ((f (ftn-tmp "cmd.f")))
    (with-open-file (s f :direction :output) (write-string (ftn-lines "      PROGRAM CMDFILE" "      PRINT *, 'compiled by command'" "      END") s))
    (multiple-value-bind (wm lw) (make-test-wm 40 100)
      (run-script wm (script-lines (format nil ":compile fortran ~A" f) (format nil ":compile fortran file ~A" f) ":execute fortran cmdfile"))
      (is (not (pane-has lw "Unknown command")))
      (is (pane-has lw "cmdfile compiled"))
      (is (pane-has lw "compiled by command")))
    (delete-file f)))

;;; ---- FORTRAN mode ------------------------------------------------------------------------------------------------

(defun ftn-editor ()
  (multiple-value-bind (wm ed) (make-editor-wm)
    (setf (buffer-mode (ed-buffer ed)) 'fortran-mode)
    (values wm ed (ed-buffer ed))))

(defun ftn-point (b) (cons (point-line b) (point-col b)))

(defparameter *ftn-demo-buffer* "      PROGRAM DEMO
      INTEGER I
      DO 10 I = 1, 3
         IF (I .GT. 1) THEN
            PRINT *, I
         ELSE
            PRINT *, 'one'
         ENDIF
10    CONTINUE
      END")

(deftest fortran-mode-basics
  (is-equal 'fortran-mode (file-mode-for "/tmp/x/solve.f"))
  (is-equal 'fortran-mode (file-mode-for "prog.for"))
  (is-equal 'fortran-mode (file-mode-for "prog.FTN"))
  (is-equal 'lisp-mode (file-mode-for "prog.lisp"))
  (is-equal '((0 11 :comment)) (f77-line-syntax "C a comment" nil))
  (is-equal '((0 5 :constant) (6 11 :builtin) (12 17 :string) (18 23 :comment))
            (f77-line-syntax "  100 PRINT 'a b' ! foo" nil))
  (is-equal '((6 13 :builtin) (14 17 :defname) (18 22 :builtin)) (f77-line-syntax "      PROGRAM FOO DATA" nil))
  (is-equal '((5 6 :builtin)) (f77-line-syntax "     $ABC" nil))
  (multiple-value-bind (wm ed b) (ftn-editor)
    ;; LINE (C-j) indents by nesting; a label typed in the statement field moves to the label field
    (dolist (l '("PROGRAM DEMO" "INTEGER I" "DO 10 I = 1, 3" "IF (I .GT. 1) THEN" "PRINT *, I" "ELSE" "PRINT *, 'one'" "ENDIF"))
      (when (member l '("PROGRAM DEMO") :test #'string=) (ed-keys wm "<tab>"))
      (ed-type wm l) (ed-keys wm "C-j"))
    (ed-type wm "10 CONTINUE") (ed-keys wm "<tab> C-j")
    (ed-type wm "END") (ed-keys wm "<tab>")
    (is-equal *ftn-demo-buffer* (buffer-string b))
    (is-equal "FORTRAN" (mode-display-name (buffer-mode b)))
    (is (plusp (count-screen-rows wm "(FORTRAN")))
    ;; Tab in the label field pads to the statement field
    (buffer-set-text b "") (ed-keys wm "<tab>")
    (is-equal '(0 . 6) (ftn-point b))
    (is-equal "      " (buf-line b 0))
    (buffer-set-text b "") (ed-type wm "12") (ed-keys wm "<tab>")
    (is-equal '(0 . 6) (ftn-point b))
    (is-equal "12    " (buf-line b 0))
    ;; Control-LINE opens a continuation card; C-^ merges it back
    (buffer-set-text b "      X = 1") (buf-goto b 0 9)
    (ed-keys wm "C-M-j")
    (is-equal "     $" (buf-line b 1))
    (is-equal '(1 . 6) (ftn-point b))
    (ed-type wm "+ 2")
    (is-equal "     $+ 2" (buf-line b 1))
    (is-equal 1 (length (f77-read-statements (buffer-string b))))
    (ed-keys wm "C-^")
    (is-equal "      X = 1+ 2" (buf-line b 0))
    (is-equal 1 (buf-nlines b))))

(deftest fortran-mode-electric-wrap
  (multiple-value-bind (wm ed b) (ftn-editor)
    (ed-keys wm "M-x") (ed-type wm "electric fortran mode") (ed-keys wm "RET")
    (is (member 'electric (buffer-minor-modes b)))
    (is (ftn-has (ed-message ed) "column 40"))
    (is (plusp (count-screen-rows wm "(FORTRAN ELECTRIC)")))
    ;; text past the wraparound column continues on a continuation card, with the continuation character
    (ed-keys wm "<tab>")
    (ed-type wm "CALL SUB(ALPHA, BETA, GAMMA, DELTA, EPSILON, ZETA, ETA)")
    (is-equal "      CALL SUB(ALPHA, BETA, GAMMA," (buf-line b 0))
    (is-equal "     $DELTA, EPSILON, ZETA, ETA)" (buf-line b 1))
    (is-equal '(1 . 32) (ftn-point b))
    (is-equal 1 (length (f77-read-statements (buffer-string b))))
    (is-equal "CALLSUB(ALPHA,BETA,GAMMA,DELTA,EPSILON,ZETA,ETA)" (%f7-squeeze (fs-text (car (f77-read-statements (buffer-string b))))))
    ;; the variables are user-settable
    (setq f77:wraparound-column 30 f77:continuation-character "+")
    (buffer-set-text b "") (ed-keys wm "<tab>")
    (ed-type wm "X = AAAA + BBBB + CCCC + DDDD")
    (is-equal "      X = AAAA + BBBB + CCCC +" (buf-line b 0))
    (is-equal "     +DDDD" (buf-line b 1))
    ;; a wrap never goes beyond column 72, and never splits a character constant before then
    (setq f77:wraparound-column 100)
    (buffer-set-text b "") (ed-keys wm "<tab>")
    (ed-type wm "PRINT *, 'THIS IS A VERY LONG CHARACTER CONSTANT THAT RUNS PAST THE LIMIT'")
    (is (<= (length (buf-line b 0)) 72))
    (is-equal "      PRINT *," (buf-line b 0))
    (is-equal "THIS IS A VERY LONG CHARACTER CONSTANT THAT RUNS PAST THE LIMIT'" (subseq (buf-line b 1) 7))
    (setq f77:wraparound-column 40 f77:continuation-character "$")
    ;; comment lines never wrap
    (buffer-set-text b "") (ed-type wm "C this is a comment line that is much longer than forty columns wide")
    (is-equal 1 (buf-nlines b))))

(deftest fortran-mode-language-units
  (multiple-value-bind (wm ed b) (ftn-editor)
    (declare (ignore ed))
    (buffer-set-text b (ftn-lines "      PROGRAM A" "      X = 1" "      END" "      SUBROUTINE B(Y)" "      Y = 2" "      Z = 3" "      END" "      FUNCTION C(Q)" "      C = Q" "      END"))
    (buf-goto b 5 8)
    (ed-keys wm "C-M-a") (is-equal '(3 . 0) (ftn-point b))                       ; the start of SUBROUTINE B
    (ed-keys wm "C-M-e") (is-equal '(6 . 9) (ftn-point b))
    (ed-keys wm "C-M-e") (is-equal '(9 . 9) (ftn-point b))
    (buf-goto b 5 8) (ed-keys wm "C-M-f") (is-equal '(6 . 0) (ftn-point b))     ; the next statement
    (ed-keys wm "C-M-b") (is-equal '(5 . 0) (ftn-point b))
    (ed-keys wm "C-M-b") (is-equal '(4 . 0) (ftn-point b))
    ;; C-M-h marks the routine around point
    (buf-goto b 5 8) (ed-keys wm "C-M-h")
    (is-equal '(3 . 0) (ftn-point b))
    (is-equal '(6 . 9) (buffer-mark b))
    ;; List Definitions
    (ed-keys wm "M-x") (ed-type wm "list definitions") (ed-keys wm "RET")
    (is (ftn-has (buffer-string (ed-buffer (find-if (lambda (w) (typep w 'editor-window)) (wm-windows wm)))) "program a"))))

(deftest fortran-mode-compiling
  (multiple-value-bind (wm ed b) (ftn-editor)
    (buffer-set-text b (ftn-lines "C -*- Mode: FORTRAN; Package: FTN-ED -*-" "      PROGRAM EDPROG" "      PRINT *, 'edited'" "      END"
                                  "      SUBROUTINE EDSUB(N)" "      INTEGER N" "      N = N + 1" "      END"))
    (f77-reset-baselines b)
    (buf-goto b 2 8)
    ;; C-c C-c compiles the routine at point; C-c C-a shows its arguments
    (ed-keys wm "C-c C-c")
    (is-equal "edprog compiled" (ed-message ed))
    (is-equal (format nil " edited~%") (let ((out (make-string-output-stream))) (f77-execute 'ftn-ed::edprog :output out) (get-output-stream-string out)))
    (is-equal nil (find-symbol "EDSUB" "FTN-ED"))
    (buf-goto b 6 8) (ed-keys wm "C-c C-a")
    (is (ftn-has (ed-message ed) "is not compiled"))
    (ed-keys wm "C-c C-b")
    (is-equal "edprog, edsub compiled" (ed-message ed))
    (ed-keys wm "C-c C-a")
    (is-equal "subroutine edsub(INTEGER)" (ed-message ed))
    ;; Compile Changed Definitions: only what changed since it was compiled
    (ed-keys wm "M-x") (ed-type wm "compile changed definitions of buffer") (ed-keys wm "RET")
    (is-equal "No definitions changed" (ed-message ed))
    (buf-goto b 6 11) (buf-insert b "0")
    (ed-keys wm "M-x") (ed-type wm "list changed definitions") (ed-keys wm "RET")
    (is-equal 1 (length (poss-entries (ed-possibilities ed))))
    (ed-switch-to-buffer ed b)
    (ed-keys wm "M-x") (ed-type wm "compile changed definitions of buffer") (ed-keys wm "RET")
    (is-equal "edsub compiled" (ed-message ed))
    (is-equal nil (f77-changed-units b :compiled))
    ;; since the file was read (numeric argument 1): everything that differs from the first text
    (is-equal 1 (length (f77-changed-units b :read)))
    (ed-switch-to-buffer ed b)
    ;; Execute FORTRAN Program runs the program with output in a buffer (there is no Listener here)
    (buf-goto b 2 8) (ed-keys wm "C-c C-x")
    (is (ftn-has (buffer-string (ed-get-buffer ed "*FORTRAN Output*")) "edited"))
    ;; Compile and Execute (the output buffer is now current: go back to the source)
    (ed-switch-to-buffer ed b)
    (ed-keys wm "C-c C-e")
    (ed-switch-to-buffer ed b)
    ;; warnings: C-c C-b reports them, C-. visits them
    (setq *f77-compiler-warnings* nil)
    (buffer-set-text b (ftn-lines "      PROGRAM W" "      INTEGER I" "      I = 'no'" "      J = 1 + 'x'" "      END"))
    (ed-keys wm "C-c C-b")
    (is (ftn-has (ed-message ed) "2 compiler warnings; line 3"))
    (buf-goto b 0 0)
    (ed-keys wm "C-.") (is-equal 2 (point-line b))
    (ed-keys wm "C-.") (is-equal 3 (point-line b))
    (buffer-set-text b (ftn-lines "      PROGRAM W" "      I = 1" "      END"))
    (ed-keys wm "C-c C-b")
    (is-equal "w compiled" (ed-message ed))
    (is-equal nil (f77-compiler-warnings "work"))))

(deftest fortran-mode-attributes-and-commands
  (multiple-value-bind (wm ed b) (ftn-editor)
    (buffer-set-text b (ftn-lines "      PROGRAM P" "      END"))
    (%f77-set-attributes b :package "MYFTN")
    (is-equal "C -*- Mode: FORTRAN; Package: MYFTN -*-" (buf-line b 0))
    (is-equal "MYFTN" (%f77-buffer-package b))
    (is-equal '("FORTRAN" "MYFTN") (let ((a (f77-buffer-attributes b))) (list (getf a :mode) (getf a :package))))
    ;; Update and Reparse Attribute List, Set Package
    (ed-keys wm "M-x") (ed-type wm "update attribute list") (ed-keys wm "RET")
    (is-equal "C -*- Mode: FORTRAN; Package: MYFTN -*-" (ed-message ed))
    (ed-keys wm "M-x") (ed-type wm "reparse attribute list") (ed-keys wm "RET")
    (is (ftn-has (ed-message ed) "package MYFTN"))
    (ed-keys wm "M-x") (ed-type wm "set package") (ed-keys wm "RET") (ed-type wm "NEWFTN") (ed-keys wm "RET")
    (is-equal "NEWFTN" (%f77-buffer-package b))
    (is (find-package "NEWFTN"))
    ;; Replace Tabs and sequence numbers
    (buffer-set-text b (format nil "	X = 1~%10	Y = 2~%~72A~A~%" "      Z = 3" "00000030"))
    (ed-keys wm "M-x") (ed-type wm "replace tabs") (ed-keys wm "RET")
    (is-equal "      X = 1" (buf-line b 0))
    (is-equal "10    Y = 2" (buf-line b 1))
    (buf-goto b 0 0)
    (ed-keys wm "M-x") (ed-type wm "find next fortran sequence number") (ed-keys wm "RET")
    (is-equal '(2 . 72) (ftn-point b))
    (ed-keys wm "M-x") (ed-type wm "remove sequence numbers") (ed-keys wm "RET")
    (is-equal "      Z = 3" (buf-line b 2))
    ;; Edit Definition (M-.) finds a routine in this buffer
    (buffer-set-text b (ftn-lines "      PROGRAM P" "      CALL SUBX" "      END" "      SUBROUTINE SUBX" "      END"))
    (buf-goto b 1 12) (ed-keys wm "M-.")
    (is-equal '(3 . 0) (ftn-point b))
    ;; breakpoints are recorded
    (setq *f77-breakpoints* nil)
    (buf-goto b 1 8) (ed-keys wm "M-x") (ed-type wm "set breakpoint") (ed-keys wm "RET")
    (is-equal 1 (length *f77-breakpoints*))
    (ed-keys wm "M-x") (ed-type wm "clear all breakpoints") (ed-keys wm "RET")
    (is-equal nil *f77-breakpoints*)
    ;; the commands shared with other modes still work in them
    (ed-switch-to-buffer ed (ed-get-buffer ed "*scratch*" t))
    (is (not (%f77-buffer-p (ed-buffer ed))))))
