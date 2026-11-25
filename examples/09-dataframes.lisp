;;; Data frames: build, write CSV, read it back, filter, group and join.
;;; expect: mean-score
(let* ((d (make-dframe '("patient" "arm" "age" "score")
                       (list '("p1" "p2" "p3" "p4" "p5" "p6") '("drug" "drug" "drug" "placebo" "placebo" "placebo")
                             '(34 41 29 52 47 38) '(7.5 8.1 6.9 5.2 5.9 6.1)))))
  (write-csv d "/tmp/ng-example-trial.csv")
  (let ((back (read-csv "/tmp/ng-example-trial.csv")))
    (format t "~A~%" (df-string back))
    (format t "older than 40:~%~A~%" (df-string (df-filter back (lambda (r) (> (funcall r "age") 40)))))
    (format t "by arm:~%~A~%" (df-string (df-summarize back '("arm") (list "n" #'length "score") (list "mean-score" #'mean "score") (list "sd" #'std-dev "score"))))
    (format t "t test drug vs placebo: p = ~,4F~%"
            (htest-p-value (t-test (df-col (df-filter back (lambda (r) (string= (funcall r "arm") "drug"))) "score")
                                   :y (df-col (df-filter back (lambda (r) (string= (funcall r "arm") "placebo"))) "score"))))
    (delete-file "/tmp/ng-example-trial.csv")))
