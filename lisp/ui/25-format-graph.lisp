;;;; 25-format-graph.lisp -- FORMAT-TEXTUAL-LIST and FORMAT-GRAPH-FROM-ROOT.
;;;;
;;;; Genera's output helpers for lists in prose ("a, b, and c") and for trees and graphs drawn with lines.  In a
;;;; Listener window each element or node is printed as a presentation of itself, so it can be clicked.

(in-package :cl)

(defun %present-with (stream object printer)
  "Print OBJECT on STREAM with PRINTER (a function of the object and the stream), as a presentation if STREAM is a pane."
  (if (typep stream 'pane-stream)
      (with-output-as-presentation (stream object (presentation-type-of object)) (funcall printer object stream))
      (funcall printer object stream)))

(defun format-textual-list (sequence printer &key (stream *standard-output*) (separator ", ") (finally "")
                                                  conjunction (if-two nil if-two-p) before-each after-each)
  "Print the elements of SEQUENCE with PRINTER (a function of the element and the stream), separated by SEPARATOR,
with CONJUNCTION (such as \"and\") before the last one, then FINALLY:
  (format-textual-list '(1 2 3) #'princ :conjunction \"and\")  prints  1, 2, and 3
Two elements are joined by \" CONJUNCTION \" (or IF-TWO).  BEFORE-EACH and AFTER-EACH are printed around each."
  (let* ((own (null stream))
         (stream (cond (own (make-string-output-stream)) ((eq stream t) *standard-output*) (t stream)))
         (items (coerce sequence 'list)) (n (length items)) (i 0))
    (dolist (x items)
      (when (> i 0)
        (cond ((and (= n 2) (or if-two-p conjunction)) (write-string (if if-two-p if-two (format nil " ~A " conjunction)) stream))
              ((and conjunction (= i (- n 1))) (write-string separator stream) (write-string conjunction stream) (write-char #\Space stream))
              (t (write-string separator stream))))
      (when before-each (write-string before-each stream))
      (%present-with stream x printer)
      (when after-each (write-string after-each stream))
      (setq i (+ i 1)))
    (write-string finally stream)
    (if own (get-output-stream-string stream) nil)))

;;; ---- graphs ----------------------------------------------------------------------------------------------

(defstruct (%gnode (:constructor %make-gnode (object label children duplicate)) (:copier nil))
  object label children duplicate (width 0) (x 0))

(defun %graph-tree (root printer inferior-producer key test duplicates depth-limit)
  "The tree of ROOT: each node's label is what PRINTER prints for it; a node seen before is a leaf marked duplicate."
  (let ((seen (make-hash-table :test (if (member test (list #'eq 'eq)) 'eq 'equal))))
    (labels ((label (obj) (let ((s (make-string-output-stream))) (funcall printer obj s) (substitute #\Space #\Newline (get-output-stream-string s))))
             (build (obj depth)
               (let* ((k (funcall key obj)) (dup (and (not duplicates) (gethash k seen))))
                 (setf (gethash k seen) t)
                 (%make-gnode obj (label obj)
                              (if (or dup (and depth-limit (>= depth depth-limit))) nil
                                  (mapcar (lambda (c) (build c (+ depth 1))) (funcall inferior-producer obj)))
                              dup))))
      (build root 0))))

(defun %print-label (stream node printer)
  (if (%gnode-duplicate node)
      (progn (%present-with stream (%gnode-object node) printer) (write-string " ..." stream))
      (%present-with stream (%gnode-object node) printer)))

(defun %graph-sideways (stream node printer prefix last root)
  ;; root
  ;; ├── child
  ;; │   └── grandchild
  ;; └── child
  (unless root
    (write-string prefix stream)
    (write-string (utf8-from-codes (if last '(#x2514 #x2500 #x2500 32) '(#x251C #x2500 #x2500 32))) stream))
  (%print-label stream node printer)
  (terpri stream)
  (let ((kids (%gnode-children node)) (sub (if root "" (%string-concat prefix (if last "    " (utf8-from-codes '(#x2502 32 32 32)))))))
    (loop for rest on kids do (%graph-sideways stream (car rest) printer sub (null (cdr rest)) nil))))

(defun %layout-down (node gap)
  "Compute the widths of the subtrees of NODE for a top-down drawing."
  (let ((lw (utf8-length (%gnode-label node))) (kids (%gnode-children node)))
    (dolist (k kids) (%layout-down k gap))
    (setf (%gnode-width node)
          (max lw (if kids (+ (reduce #'+ (mapcar #'%gnode-width kids)) (* gap (- (length kids) 1))) 0)))))

(defun %place-down (node x gap)
  (setf (%gnode-x node) x)
  (let ((cx x))
    (dolist (k (%gnode-children node)) (%place-down k cx gap) (setq cx (+ cx (%gnode-width k) gap)))))

(defun %node-center (node) (+ (%gnode-x node) (floor (%gnode-width node) 2)))

(defun %graph-down (stream root printer gap)
  ;;      root
  ;;   ┌───┴───┐
  ;;   a       b
  (%layout-down root gap)
  (%place-down root 0 gap)
  (let ((level (list root)))
    (loop
      (when (null level) (return))
      ;; the labels of this level, centred over their subtrees
      (let ((col 0))
        (dolist (n level)
          (let* ((lw (utf8-length (%gnode-label n))) (start (max col (- (%node-center n) (floor lw 2)))))
            (dotimes (i (- start col)) (write-char #\Space stream))
            (%print-label stream n printer)
            (setq col (+ start lw)))))
      (terpri stream)
      (let ((next (mapcan (lambda (n) (copy-list (%gnode-children n))) level)))
        (when next
          ;; connectors: a ┴/│ under each parent, a ┬ (or ┌ ┐) over each child, ─ between
          (let* ((width (reduce #'max (mapcar (lambda (n) (+ (%gnode-x n) (%gnode-width n))) level)))
                 (line (make-array (+ width 2) :initial-element 32)))
            (dolist (n level)
              (let ((kids (%gnode-children n)))
                (when kids
                  (let* ((pc (%node-center n)) (cs (mapcar #'%node-center kids)) (lo (min pc (car cs))) (hi (max pc (car (last cs)))))
                    (loop for c from lo to hi do (setf (aref line c) #x2500))
                    (dolist (c cs) (setf (aref line c) #x252C))
                    (when (= (length cs) 1) (setf (aref line (car cs)) #x2502))
                    (when (> (length cs) 1)
                      (setf (aref line (car cs)) (if (= (car cs) pc) #x251C #x250C)
                            (aref line (car (last cs))) (if (= (car (last cs)) pc) #x2524 #x2510)))
                    (unless (or (= (length cs) 1) (member pc cs)) (setf (aref line pc) #x2534))
                    (when (and (> (length cs) 1) (member pc cs) (not (= pc (car cs))) (not (= pc (car (last cs)))))
                      (setf (aref line pc) #x253C))))))
            (write-string (string-right-trim " " (utf8-from-codes (coerce line 'list))) stream)
            (terpri stream)))
        (setq level next)))))

(defun format-graph-from-root (root printer inferior-producer &key (stream *standard-output*) (orientation :horizontal)
                                                                  (key #'identity) (test #'eql) dont-draw-duplicates
                                                                  depth-limit (gap 2))
  "Draw the graph reachable from ROOT: PRINTER (a function of a node and the stream) prints each node and
INFERIOR-PRODUCER returns a node's children.  ORIENTATION :HORIZONTAL draws it sideways, as an outline with lines;
:VERTICAL draws it top-down.  A node reached again (compared by KEY and TEST) is drawn again followed by \"...\",
without its children, so shared nodes and cycles are drawn once.  DEPTH-LIMIT stops the drawing at that depth.
With STREAM NIL the drawing is returned as a string.
  (format-graph-from-root 'standard-object (lambda (c s) (princ c s))
                          (lambda (c) (mapcar #'class-name (class-direct-subclasses (find-class c)))))"
  (declare (ignore dont-draw-duplicates))
  (let* ((own (null stream))
         (stream (cond (own (make-string-output-stream)) ((eq stream t) *standard-output*) (t stream)))
         (tree (%graph-tree root printer inferior-producer key test nil depth-limit)))
    (unless own (fresh-line stream))
    (if (eq orientation :vertical)
        (%graph-down stream tree printer gap)
        (%graph-sideways stream tree printer "" t t))
    (if own (get-output-stream-string stream) (values))))
