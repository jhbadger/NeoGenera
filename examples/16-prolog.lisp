;;; The Prolog Tool Kit: Prolog compiled into the running world, sharing Lisp's terms, integers and functions.
;;; expect: The zebra belongs to the japanese
(prolog:consult "examples/prolog/family.pl")
(prolog:consult "examples/prolog/queens.pl")
(prolog:consult "examples/prolog/expert.pl")
(prolog:consult "examples/prolog/expr.pl")
(prolog:consult "examples/prolog/dna.pl")
(prolog:consult "examples/prolog/zebra.pl")

;; prolog:query returns one alist (variable . value) per solution; values are converted to Lisp.
;; The value of the first variable in the first solution of a query.
(defun qv (goal) (cdr (caar (prolog:query goal :limit 1))))

(format t "~&Children of tom: ~{~A~^ ~}~%" (mapcar #'cdar (prolog:query "parent(tom, X)")))
(format t "Descendants of tom: ~A~%" (cdr (assoc "Ds" (car (prolog:query "descendants(tom, Ds)")) :test #'string=)))
(format t "Siblings of ann: ~A~%" (mapcar #'cdar (prolog:query "sibling(ann, X)")))
(format t "Is tom an ancestor of jim? ~A~%" (prolog:prove "ancestor(tom, jim)"))

;; Backtracking enumerates solutions; prolog:do-query runs a body for each one.
(prolog:do-query (b "grandparent(G, C)")
  (format t "  ~A is a grandparent of ~A~%" (cdr (assoc "G" b :test #'string=)) (cdr (assoc "C" b :test #'string=))))

(format t "Eight queens: ~A solutions, the first ~A~%"
        (qv "count_solutions(8, N)")
        (qv "queens(8, Qs)"))

;; Rules that chain backwards from observed features.
(format t "A tawny animal with dark spots that eats meat and has hair is a ~A~%"
        (qv "identify([hair, eats_meat, tawny_colour, dark_spots], A)"))

;; A grammar evaluates arithmetic with exact integers and rationals turned to floats.
(format t "2+3*(4-1) = ~A, and 7/2 = ~A~%" (qv "calc(\"2+3*(4-1)\", V)") (qv "calc(\"7/2\", V)"))

;; DNA: a grammar translates; the Lisp bioinformatics functions are called from Prolog.
(format t "ATGGCCATTGTAATGGGCCGCTGA translates to ~A~%" (qv "translate(\"ATGGCCATTGTAATGGGCCGCTGA\", P)"))
(format t "Its other strand reads ~A~%" (qv "reverse_strand(\"ATGGCCATTGTA\", R)"))

;; Prolog calls Lisp, and Lisp defines Prolog predicates.
(format t "From Prolog: lisp/2 says ~A~%" (qv "lisp(X, '(expt 2 100)')"))
(prolog:define-predicate "lisp_square" 2
  (lambda (x y) (and (numberp (prolog:deref x)) (prolog:unify y (* (prolog:deref x) (prolog:deref x))))))
(format t "A predicate written in Lisp: 12 squared is ~A~%" (qv "lisp_square(12, S)"))

;; Integers never overflow, and the logical update view keeps database loops finite.
(format t "2 ** 200 = ~A~%" (qv "X is 2 ** 200"))

;; The zebra puzzle exercises unification and backtracking hard.
(let ((answer (car (prolog:query "zebra(Owner, Water)"))))
  (format t "The zebra belongs to the ~(~A~) and the ~(~A~) drinks water~%"
          (cdr (assoc "Owner" answer :test #'string=)) (cdr (assoc "Water" answer :test #'string=))))
