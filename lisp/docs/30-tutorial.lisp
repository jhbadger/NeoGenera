;;;; 30-tutorial.lisp -- guided interactive lessons that run in the Listener.
;;;;
;;;; A lesson is a list of steps; each step has instruction text, an optional CHECK (an expression
;;;; over FORM, VALUE and VALUES, evaluated after the learner's form runs) and a hint.  The
;;;; Listener calls TUTORIAL-OBSERVE after each successful evaluation.  Commands: :tutorial [name],
;;;; :lessons, :hint, :skip, :end-tutorial.

(in-package :cl)

(defstruct (lesson (:conc-name lesson-) (:constructor %make-lesson) (:copier nil)) name title intro steps)
(defvar *lessons* nil)

(defun %register-lesson (name title intro steps)
  (let ((l (%make-lesson :name name :title title :intro intro :steps steps)))
    (setq *lessons* (append (remove name *lessons* :key #'lesson-name) (list l)))
    l))

(defmacro deflesson (name title intro &rest steps)
  "STEPS: each a plist (:text \"...\" :check expression :hint \"...\"); CHECK may use FORM, VALUE, VALUES."
  `(%register-lesson ',name ,title ,intro
                     (list ,@(mapcar (lambda (st)
                                       `(list ,(getf st :text)
                                              ,(if (getf st :check)
                                                   `(lambda (form value values) (declare (ignorable form value values)) ,(getf st :check))
                                                   nil)
                                              ,(getf st :hint)))
                                     steps))))

(defun find-lesson (x)
  (cond ((lesson-p x) x)
        ((integerp x) (nth (- x 1) *lessons*))
        ((stringp x) (or (find-if (lambda (l) (string-equal (symbol-name (lesson-name l)) x)) *lessons*)
                         (let ((n (parse-integer x :junk-allowed t))) (and n (nth (- n 1) *lessons*)))))
        (t (find x *lessons* :key #'lesson-name))))

(defun tutorial-say (w style fmt &rest args)
  (let ((out (lw-stream w))) (with-style (out style) (apply #'format out fmt args))))

(defun tutorial-show-step (w)
  (let* ((st (lw-lesson w)) (lesson (car st)) (i (cadr st)) (step (nth i (lesson-steps lesson))))
    (tutorial-say w :bold "~&Step ~D of ~D: " (+ i 1) (length (lesson-steps lesson)))
    (tutorial-say w :normal "~A~%" (first step))))

(defun tutorial-start (w lesson)
  (setf (lw-lesson w) (list lesson 0 0))
  (tutorial-say w :bold "~&~%Lesson: ~A~%" (lesson-title lesson))
  (tutorial-say w :dim "~A~%" (lesson-intro lesson))
  (tutorial-show-step w))

(defun tutorial-advance (w)
  (let* ((st (lw-lesson w)) (lesson (car st)))
    (if (< (+ 1 (cadr st)) (length (lesson-steps lesson)))
        (progn (setf (cadr st) (+ 1 (cadr st)) (caddr st) 0) (tutorial-show-step w))
        (let ((next (let ((m (member lesson *lessons*))) (cadr m))))
          (setf (lw-lesson w) nil)
          (tutorial-say w :bold "~&Lesson complete: ~A~%" (lesson-title lesson))
          (if next (tutorial-say w :dim "Next: :tutorial ~A  (~A)~%" (string-downcase (symbol-name (lesson-name next))) (lesson-title next))
              (tutorial-say w :dim "That was the last lesson. :doc user-guide continues from here.~%"))))))

(defun tutorial-observe (w form vals)
  "Called after the learner's FORM evaluated to VALS."
  (let* ((st (lw-lesson w)) (step (nth (cadr st) (lesson-steps (car st)))) (check (second step)))
    (if (or (null check) (funcall check form (car vals) vals))
        (progn (tutorial-say w :value "~&Correct.~%") (tutorial-advance w))
        (progn (setf (caddr st) (+ 1 (caddr st)))
               (tutorial-say w :dim "~&Not quite. ~A~%" (if (>= (caddr st) 2) "Type :hint for a hint, or :skip to move on." "Try again."))))))

(defun %current-lesson-listener ()
  (or (and (typep *current-listener* 'listener-window) *current-listener*) (error "No Listener window")))

(define-command (tutorial :documentation "Start an interactive lesson (:lessons lists them)") ((name word :default ""))
  (let* ((w (%current-lesson-listener)) (l (if (string= name "") (car *lessons*) (find-lesson name))))
    (unless l (error "No lesson named ~A (try :lessons)" name))
    (tutorial-start w l)))
(define-command (lessons :documentation "List the tutorial lessons") ()
  (let ((i 0))
    (dolist (l *lessons*)
      (setq i (+ i 1))
      (format t "  ~D. ~A (:tutorial ~A) -- ~A~%" i (string-downcase (symbol-name (lesson-name l))) i (lesson-title l)))))
(define-command (hint :documentation "Show a hint for the current tutorial step") ()
  (let* ((w (%current-lesson-listener)) (st (lw-lesson w)))
    (if st (tutorial-say w :dim "~A~%" (or (third (nth (cadr st) (lesson-steps (car st)))) "No hint for this step; read the instruction again."))
        (format t "~&No lesson in progress (:lessons)~%"))))
(define-command (skip :documentation "Skip the current tutorial step") ()
  (let* ((w (%current-lesson-listener)))
    (if (lw-lesson w) (tutorial-advance w) (format t "~&No lesson in progress~%"))))
(define-command (end-tutorial :documentation "Leave the current lesson") ()
  (let ((w (%current-lesson-listener))) (setf (lw-lesson w) nil) (format t "~&Lesson ended~%")))

(deflesson first-steps "First steps: evaluating expressions"
  "Type each expression at the prompt and press Return. Lisp prints the value after =>."
  (:text "Add two numbers: type (+ 2 3)." :check (eql value 5) :hint "Type exactly (+ 2 3) and press Return.")
  (:text "Lisp keeps exact fractions. Evaluate (/ 1 3)." :check (equal value 1/3) :hint "Type (/ 1 3); the answer is the ratio 1/3, not 0.333.")
  (:text "Integers never overflow. Try (expt 2 100)." :check (and (integerp value) (> value 1000000)) :hint "Type (expt 2 100)."))

(deflesson lists "Lists and symbols"
  "Lists are the basic data structure. A quote stops Lisp from evaluating what follows."
  (:text "Build a list: (list 1 2 3)." :check (equal value '(1 2 3)) :hint "Type (list 1 2 3).")
  (:text "Take the first element of a quoted list: (car '(a b c))." :check (and (symbolp value) (string= (symbol-name value) "A")) :hint "Do not forget the quote before the list.")
  (:text "Apply a function to every element: (mapcar #'1+ '(1 2 3))." :check (equal value '(2 3 4)) :hint "#'1+ names the function that adds one."))

(deflesson functions "Defining functions"
  "DEFUN defines a function; the Listener shows its name."
  (:text "Define a function: (defun square (x) (* x x))." :check (and (symbolp value) (string= (symbol-name value) "SQUARE")) :hint "(defun square (x) (* x x))")
  (:text "Call it: (square 12)." :check (eql value 144) :hint "Type (square 12).")
  (:text "Map it over a list: (mapcar #'square '(1 2 3))." :check (equal value '(1 4 9)) :hint "(mapcar #'square '(1 2 3))"))

(deflesson objects "Objects with CLOS"
  "CLOS classes have slots; generic functions dispatch on the classes of their arguments."
  (:text "Define a class: (defclass point () ((x :initarg :x :initform 0 :accessor point-x)))."
   :check (and (symbolp value) (string= (symbol-name value) "POINT")) :hint "Copy the form exactly; the :accessor option creates POINT-X.")
  (:text "Make an instance: (setq p (make-instance 'point :x 3))." :check (typep value 'standard-object) :hint "(setq p (make-instance 'point :x 3))")
  (:text "Read the slot: (point-x p)." :check (eql value 3) :hint "(point-x p)"))

(deflesson errors "Errors and conditions"
  "Errors are conditions. HANDLER-CASE catches them; unhandled errors show a backtrace you can click."
  (:text "Catch an error: (handler-case (car 5) (error (e) (princ-to-string e)))." :check (stringp value) :hint "Type the whole form on one line.")
  (:text "Catch a division by zero: (handler-case (/ 1 0) (division-by-zero () :oops))." :check (eq value :oops) :hint "(handler-case (/ 1 0) (division-by-zero () :oops))"))

(deflesson environment "Exploring the environment"
  "NeoGenera documents itself. Try these and read what appears; any input moves you on."
  (:text "Read about a function: (doc 'mapcar).")
  (:text "Describe an object: (describe 42).")
  (:text "Open the manual: type :doc and click a topic. :search-doc editor finds topics by words. Evaluate (+ 1 1) when you are done."))
