;;;; 20-export.lisp -- render the documentation to HTML and PDF from the same nodes the viewer uses.
;;;;
;;;; EXPORT-DOCUMENTATION writes index.html, one HTML file per document, reference.html (an API
;;;; reference generated from docstrings and lambda lists) and manual.pdf.  The PDF writer is
;;;; self-contained: monospace text in Courier / Courier-Bold, wrapped and paginated.

(in-package :cl)

;;; ---- HTML ---------------------------------------------------------------------------------------------

(defun html-escape (s)
  (let ((out (make-string-output-stream)))
    (dotimes (i (length s))
      (let ((c (char s i)))
        (cond ((char= c #\&) (write-string "&amp;" out)) ((char= c #\<) (write-string "&lt;" out))
              ((char= c #\>) (write-string "&gt;" out)) ((char= c #\") (write-string "&quot;" out))
              (t (write-char c out)))))
    (get-output-stream-string out)))

(defun %anchor (sym)
  (let ((out (make-string-output-stream)))
    (dotimes (i (length (symbol-name sym)))
      (let ((c (char (symbol-name sym) i)))
        (if (and (< (char-code c) 128) (or (alphanumericp c) (char= c #\-))) (write-char (char-downcase c) out) (format out "_~X" (char-code c)))))
    (%string-concat "sym-" (get-output-stream-string out))))

(defun topic-file (topic) (%string-concat (string-downcase (symbol-name topic)) ".html"))

(defun inlines->html (nodes)
  (apply #'%string-concat
         (mapcar (lambda (n)
                   (cond ((stringp n) (html-escape n))
                         ((eq (car n) :code)
                          (let ((sym (%doc-link-symbol (cadr n))))
                            (if (and sym (symbol-documented-p sym))
                                (format nil "<a href=\"reference.html#~A\"><code>~A</code></a>" (%anchor sym) (html-escape (cadr n)))
                                (format nil "<code>~A</code>" (html-escape (cadr n))))))
                         ((eq (car n) :strong) (format nil "<strong>~A</strong>" (inlines->html (cdr n))))
                         ((eq (car n) :em) (format nil "<em>~A</em>" (inlines->html (cdr n))))
                         ((eq (car n) :ref)
                          (if (find-document (cadr n))
                              (format nil "<a href=\"~A\">~A</a>" (topic-file (cadr n)) (html-escape (caddr n)))
                              (html-escape (caddr n))))
                         ((eq (car n) :fn)
                          (let ((name (html-escape (string-downcase (symbol-name (cadr n))))))
                            (if (symbol-documented-p (cadr n))
                                (format nil "<a href=\"reference.html#~A\"><code>~A</code></a>" (%anchor (cadr n)) name)
                                (format nil "<code>~A</code>" name))))
                         ((eq (car n) :link) (format nil "<a href=\"~A\">~A</a>" (html-escape (cadr n)) (html-escape (caddr n))))
                         (t "")))
                 nodes)))

(defun nodes->html (nodes)
  (let ((out (make-string-output-stream)))
    (dolist (n nodes)
      (case (car n)
        (:h (format out "<h~D>~A</h~D>~%" (cadr n) (inlines->html (cddr n)) (cadr n)))
        (:p (format out "<p>~A</p>~%" (inlines->html (cdr n))))
        (:example (format out "<pre class=\"example\"><code>~A</code></pre>~%" (html-escape (cadr n))))
        ((:ul :ol) (format out "<~A>~%" (if (eq (car n) :ul) "ul" "ol"))
         (dolist (i (cdr n)) (format out "<li>~A</li>~%" (inlines->html i)))
         (format out "</~A>~%" (if (eq (car n) :ul) "ul" "ol")))
        (:table (format out "<table>~%")
         (let ((first t))
           (dolist (r (cdr n))
             (format out "<tr>~{~A~}</tr>~%"
                     (mapcar (lambda (c) (format nil "<~A>~A</~A>" (if first "th" "td") (inlines->html c) (if first "th" "td"))) r))
             (setq first nil)))
         (format out "</table>~%"))
        (:see-also (format out "<p class=\"see-also\">See also: ~{~A~^, ~}</p>~%"
                           (mapcar (lambda (x) (if (find-document x) (format nil "<a href=\"~A\">~A</a>" (topic-file x) (html-escape (doc-title (find-document x)))) (html-escape (princ-to-string x))))
                                   (cdr n))))
        (t nil)))
    (get-output-stream-string out)))

(defparameter *html-style*
  "body{font-family:Georgia,serif;max-width:46em;margin:2em auto;padding:0 1em;line-height:1.5;color:#222}
nav{font-family:sans-serif;font-size:90%;border-bottom:1px solid #ccc;padding-bottom:.5em;margin-bottom:1.5em}
nav a{margin-right:1em}h1,h2,h3{font-family:sans-serif}
pre{background:#f4f3ee;padding:.7em;overflow-x:auto;border-left:3px solid #999}code{font-family:Menlo,Consolas,monospace;font-size:92%}
table{border-collapse:collapse}td,th{border:1px solid #bbb;padding:.2em .6em;text-align:left}th{background:#eee}
.see-also{font-style:italic}.sig{font-weight:bold}")

(defun %html-page (title body)
  (format nil "<!DOCTYPE html>~%<html lang=\"en\"><head><meta charset=\"utf-8\"><title>~A</title><style>~A</style></head><body>~%<nav><a href=\"index.html\">Contents</a>~{<a href=\"~A\">~A</a>~}<a href=\"reference.html\">API reference</a></nav>~%~A</body></html>~%"
          (html-escape title) *html-style*
          (let ((acc nil)) (dolist (d *documents*) (setq acc (append acc (list (topic-file (doc-name d)) (html-escape (doc-title d)))))) acc)
          body))

(defun doc->html (doc) (%html-page (doc-title doc) (nodes->html (doc-nodes doc))))

(defun documented-symbols ()
  (let ((acc nil))
    (dolist (p (list-all-packages))
      (dolist (s (%package-symbols p nil)) (when (and (eq (symbol-package s) p) (symbol-documented-p s)) (setq acc (cons s acc)))))
    (sort (remove-duplicates acc) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun %reference-html ()
  (let ((out (make-string-output-stream)))
    (format out "<h1>API reference</h1>~%")
    (dolist (s (documented-symbols))
      (format out "<h3 id=\"~A\">~A</h3>~%<p><span class=\"sig\">~A</span> <code>~A</code></p>~%"
              (%anchor s) (html-escape (%fname s)) (html-escape (string-downcase (princ-to-string (or (car (symbol-kinds s)) :symbol))))
              (html-escape (or (arglist-string s) (string-downcase (symbol-name s)))))
      (dolist (ind '(%doc-function %doc-variable %doc-type))
        (let ((d (get s ind))) (when d (dolist (p (%doc-paragraphs d)) (format out "<p>~A</p>~%" (inlines->html (%md-inline p))))))))
    (%html-page "API reference" (get-output-stream-string out))))

(defun %index-html ()
  (let ((out (make-string-output-stream)) (cats nil))
    (format out "<h1>NeoGenera documentation</h1>~%")
    (dolist (d *documents*) (unless (member (doc-category d) cats) (setq cats (append cats (list (doc-category d))))))
    (dolist (c cats)
      (format out "<h2>~A</h2>~%<ul>~%" (string-capitalize (substitute #\Space #\- (if c (symbol-name c) "other"))))
      (dolist (d *documents*)
        (when (eq (doc-category d) c)
          (format out "<li><a href=\"~A\">~A</a> &mdash; ~A</li>~%" (topic-file (doc-name d)) (html-escape (doc-title d)) (html-escape (doc-summary d)))))
      (format out "</ul>~%"))
    (format out "<h2>Reference</h2>~%<ul><li><a href=\"reference.html\">API reference</a> &mdash; every documented function, macro, variable and class</li></ul>~%")
    (%html-page "NeoGenera documentation" (get-output-stream-string out))))

;;; ---- PDF --------------------------------------------------------------------------------------------------

(defun pdf-escape (s)
  (let ((out (make-string-output-stream)) (i 0) (n (length s)))
    (loop
      (when (>= i n) (return))
      (let ((c (char-code (char s i))))
        (cond ((or (= c 92) (= c 40) (= c 41)) (write-char #\\ out) (write-char (code-char c) out))
              ((and (>= c 32) (< c 127)) (write-char (code-char c) out))
              ((>= c 128) (write-char #\? out))
              (t (write-char #\Space out))))
      (setq i (+ i 1)))
    (get-output-stream-string out)))

(defun %pdf-style (style)
  "(font size leading) for a line style."
  (case style (:title '(2 15 20)) (:h1 '(2 13 18)) (:h2 '(2 11 15)) (:h3 '(2 10 14)) (:blank '(1 9 6)) (t '(1 9 11))))

(defun reference-lines (width)
  (let ((lines (list (cons :title "API reference") (cons :blank ""))))
    (dolist (s (documented-symbols))
      (setq lines (append lines (list (cons :h3 (%fname s))
                                      (cons :code (let ((a (or (arglist-string s) (string-downcase (symbol-name s)))))
                                                    (if (> (length a) 90) (%string-concat (subseq a 0 87) "...") a))))))
      (dolist (ind '(%doc-function %doc-variable %doc-type))
        (let ((d (get s ind)))
          (when d (dolist (p (%doc-paragraphs d))
                    (dolist (l (wrap-words (inline-text (%md-inline p)) width "  " "  ")) (setq lines (append lines (list (cons :text l)))))))))
      (setq lines (append lines (list (cons :blank "")))))
    lines))

(defun %paginate (lines)
  "Split LINES into pages of (font size x y text) entries."
  (let ((pages nil) (cur nil) (y 738))
    (dolist (ln lines)
      (let* ((st (%pdf-style (car ln))) (lead (caddr st)))
        (when (and (< (- y lead) 54) cur) (setq pages (cons (nreverse cur) pages)) (setq cur nil) (setq y 738))
        (unless (and (null cur) (eq (car ln) :blank))
          (setq y (- y lead))
          (when (> (length (cdr ln)) 0) (setq cur (cons (list (car st) (cadr st) 54 y (cdr ln)) cur))))))
    (when cur (setq pages (cons (nreverse cur) pages)))
    (or (nreverse pages) (list nil))))

(defun write-pdf (path lines &optional (title "NeoGenera documentation"))
  "Write LINES, a list of (style . text), to PATH as a PDF file."
  (declare (ignorable title))
  (let* ((pages (%paginate lines)) (npages (length pages)) (chunks nil) (off 0) (offsets nil))
    (labels ((emit (str) (setq chunks (cons str chunks)) (setq off (+ off (length str))))
             (begin-object (n) (setq offsets (cons (cons n off) offsets)) (emit (format nil "~D 0 obj~%" n))))
      (emit (format nil "%PDF-1.4~%"))
      (begin-object 1) (emit (format nil "<< /Type /Catalog /Pages 2 0 R >>~%endobj~%"))
      (begin-object 2)
      (emit (format nil "<< /Type /Pages /Count ~D /Kids [~{~D 0 R~^ ~}] >>~%endobj~%" npages
                    (let ((ids nil)) (dotimes (i npages) (setq ids (append ids (list (+ 5 (* 2 i)))))) ids)))
      (begin-object 3) (emit (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /Courier /Encoding /WinAnsiEncoding >>~%endobj~%"))
      (begin-object 4) (emit (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /Courier-Bold /Encoding /WinAnsiEncoding >>~%endobj~%"))
      (let ((i 0))
        (dolist (page pages)
          (let* ((pn (+ 5 (* 2 i))) (cn (+ pn 1))
                 (content (let ((o (make-string-output-stream)))
                            (dolist (e page)
                              (format o "BT /F~D ~D Tf 1 0 0 1 ~D ~D Tm (~A) Tj ET~%" (first e) (second e) (third e) (fourth e) (pdf-escape (fifth e))))
                            (get-output-stream-string o))))
            (begin-object pn)
            (emit (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents ~D 0 R /Resources << /Font << /F1 3 0 R /F2 4 0 R >> >> >>~%endobj~%" cn))
            (begin-object cn)
            (emit (format nil "<< /Length ~D >>~%stream~%" (length content)))
            (emit content)
            (emit (format nil "endstream~%endobj~%")))
          (setq i (+ i 1))))
      (let ((xref-off off) (count (+ 5 (* 2 npages))))
        (emit (format nil "xref~%0 ~D~%" (- count 0)))
        (emit (format nil "0000000000 65535 f ~%"))
        (dotimes (k (- count 1))
          (emit (format nil "~10,'0D 00000 n ~%" (cdr (assoc (+ k 1) offsets)))))
        (emit (format nil "trailer~%<< /Size ~D /Root 1 0 R >>~%startxref~%~D~%%%EOF~%" count xref-off))))
    (with-open-file (s path :direction :output)
      (dolist (c (reverse chunks)) (write-string c s)))
    path))

;;; ---- driver ---------------------------------------------------------------------------------------------------

(defun write-text-file (path text) (with-open-file (s path :direction :output) (write-string text s)) path)

(defun export-documentation (dir &key (pdf t))
  "Write HTML (and a PDF) for every document to DIR; returns the list of files written."
  (%make-directory dir)
  (let ((files nil))
    (flet ((out (name text) (setq files (cons (write-text-file (%string-concat dir "/" name) text) files))))
      (out "index.html" (%index-html))
      (dolist (d *documents*) (out (topic-file (doc-name d)) (doc->html d)))
      (out "reference.html" (%reference-html)))
    (when pdf
      (let ((lines nil))
        (dolist (d *documents*) (setq lines (append lines (doc->lines d 90) (list (cons :blank "") (cons :blank "")))))
        (setq lines (append lines (reference-lines 90)))
        (setq files (cons (write-pdf (%string-concat dir "/manual.pdf") lines) files))))
    (nreverse files)))

(define-command (export-docs :documentation "Write the documentation as HTML and PDF to a directory") ((dir string :prompt "Directory"))
  (let ((files (export-documentation dir)))
    (format t "~&Wrote ~D files to ~A~%" (length files) dir)))
