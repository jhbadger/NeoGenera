;;; Write documentation: docstrings, a document in the Lisp markup, search, and HTML export.
;;; expect: exported
(defun celsius->fahrenheit (c)
  "Convert the temperature C from degrees Celsius to degrees Fahrenheit."
  (+ 32 (* 9/5 c)))
(define-document :temperature-notes (:title "Temperature Notes" :keywords (temperature units) :category :guide)
  (:h 1 "Temperature Notes")
  (:p "Use " (:fn celsius->fahrenheit) " to convert. The formula is " (:code "F = 9/5 C + 32") ".")
  (:example "(celsius->fahrenheit 100)"))
(format t "~A~%" (documentation 'celsius->fahrenheit 'function))
(format t "search \"temperature\": ~{~A~^, ~}~%" (mapcar #'fourth (search-documentation "temperature")))
(doc :temperature-notes)
(let ((files (export-documentation "/tmp/ng-example-docs" :pdf nil)))
  (format t "exported ~D files, e.g. ~A~%" (length files) (first files)))
