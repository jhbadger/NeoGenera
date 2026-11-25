;;;; 40-external.lisp -- calling external tools safely, and parsing their output into objects.
;;;;
;;;; Programs are started with an argument list through execvp (never through a shell), so file names and
;;;; query strings are not interpreted by a shell.  Program names are checked, and output is parsed into
;;;; first-class objects: BLAST tabular hits, PAF alignments, samtools depth.

(in-package :cl)

(defun %safe-program-name-p (name)
  (and (> (length name) 0)
       (or (every (lambda (c) (or (alphanumericp c) (find c "-_."))) name)
           (and (char= (char name 0) #\/) (every (lambda (c) (not (find c (list #\Newline #\Nul)))) name)))))

(defun %path-directories () (let ((p (getenv "PATH"))) (if p (%split-on p #\:) (list "/usr/bin" "/bin"))))

(defun find-executable (name)
  "The full path of the program NAME found on PATH (or NAME itself when it is an existing absolute path), or NIL."
  (cond ((not (%safe-program-name-p name)) nil)
        ((char= (char name 0) #\/) (and (probe-file name) name))
        (t (dolist (d (%path-directories) nil)
             (let ((full (%string-concat d "/" name))) (when (probe-file full) (return full)))))))

(defstruct (tool-result (:copier nil) (:constructor %make-tool-result (program args output status))) program args output status)

(defmethod print-object ((r tool-result) stream)
  (format stream "#<TOOL-RESULT ~A exit ~D ~D bytes>" (tool-result-program r) (tool-result-status r) (length (tool-result-output r))))

(defun tool-ok-p (r) "True when the tool exited with status 0." (eql (tool-result-status r) 0))

(defun run-tool (program args &key input (check t))
  "Run PROGRAM (a bare name looked up on PATH, or an absolute path) with the argument list ARGS and optional INPUT text on its
standard input.  Returns a TOOL-RESULT holding the combined output and the exit status.  With CHECK an error is signalled
when the program is missing or exits non-zero."
  (unless (%safe-program-name-p program) (error "Refusing to run program named ~S" program))
  (unless (every #'stringp args) (error "Tool arguments must be strings: ~S" args))
  (let ((exe (find-executable program)))
    (unless exe (if check (error "Program ~A was not found on PATH" program) (return-from run-tool (%make-tool-result program args "" 127))))
    (multiple-value-bind (out status) (run-program exe args input)
      (when (and check (not (eql status 0))) (error "~A exited with status ~D: ~A" program status (subseq out 0 (min 300 (length out)))))
      (%make-tool-result program args out status))))

(defun tool-available-p (program) "True when PROGRAM can be found." (and (find-executable program) t))

(defun tool-lines (result) "The output lines of a TOOL-RESULT." (remove "" (split-lines (tool-result-output result)) :test #'string=))

;;; ---- BLAST tabular (outfmt 6) -------------------------------------------------------------------------------------

(defstruct (blast-hit (:copier nil) (:constructor %make-blast-hit (query subject identity length mismatches gaps qstart qend sstart send evalue bitscore)))
  query subject identity length mismatches gaps qstart qend sstart send evalue bitscore)

(defmethod print-object ((h blast-hit) stream)
  (format stream "#<BLAST ~A -> ~A ~,1F% ~D e=~A>" (blast-hit-query h) (blast-hit-subject h) (blast-hit-identity h) (blast-hit-length h) (blast-hit-evalue h)))

(defun parse-blast-tabular (lines)
  "BLAST-HIT objects from BLAST outfmt 6 (tab-separated) LINES; comment lines are skipped."
  (let ((out nil))
    (dolist (l lines)
      (let ((f (%split-on (string-right-trim (string #\Return) l) #\Tab)))
        (when (and (>= (length f) 12) (not (%starts-with-p (first f) "#")))
          (push (%make-blast-hit (nth 0 f) (nth 1 f) (float (read-from-string (nth 2 f))) (parse-integer (nth 3 f)) (parse-integer (nth 4 f)) (parse-integer (nth 5 f))
                                 (parse-integer (nth 6 f)) (parse-integer (nth 7 f)) (parse-integer (nth 8 f)) (parse-integer (nth 9 f))
                                 (float (read-from-string (nth 10 f))) (float (read-from-string (nth 11 f))))
                out))))
    (nreverse out)))

(defun blast-best-hits (hits)
  "The best (lowest e-value) hit for each query."
  (let ((best (make-hash-table :test 'equal)) (order nil))
    (dolist (h hits)
      (let ((cur (gethash (blast-hit-query h) best)))
        (unless cur (push (blast-hit-query h) order))
        (when (or (null cur) (< (blast-hit-evalue h) (blast-hit-evalue cur))) (setf (gethash (blast-hit-query h) best) h))))
    (mapcar (lambda (q) (gethash q best)) (nreverse order))))

(defun run-blast (program query-file database &key (evalue 1e-5) (max-targets 10) (extra nil))
  "Run a BLAST+ program (blastn, blastp, ...) on QUERY-FILE against DATABASE and return BLAST-HIT objects."
  (unless (member program '("blastn" "blastp" "blastx" "tblastn" "tblastx") :test #'string=) (error "Unknown BLAST program ~A" program))
  (parse-blast-tabular (tool-lines (run-tool program (append (list "-query" query-file "-db" database "-outfmt" "6" "-evalue" (format nil "~A" evalue)
                                                                   "-max_target_seqs" (format nil "~D" max-targets))
                                                             extra)))))

;;; ---- PAF (minimap2) ---------------------------------------------------------------------------------------------------------

(defstruct (paf-record (:copier nil) (:constructor %make-paf-record (qname qlen qstart qend strand tname tlen tstart tend matches block-length mapq)))
  qname qlen qstart qend strand tname tlen tstart tend matches block-length mapq)

(defun paf-identity (r) "Fraction of matching bases over the alignment block of R." (/ (float (paf-record-matches r)) (max 1 (paf-record-block-length r))))

(defun parse-paf (lines)
  "PAF-RECORD objects from PAF LINES (minimap2 output)."
  (let ((out nil))
    (dolist (l lines)
      (let ((f (%split-on (string-right-trim (string #\Return) l) #\Tab)))
        (when (>= (length f) 12)
          (push (%make-paf-record (nth 0 f) (parse-integer (nth 1 f)) (parse-integer (nth 2 f)) (parse-integer (nth 3 f)) (if (string= (nth 4 f) "+") 1 -1)
                                  (nth 5 f) (parse-integer (nth 6 f)) (parse-integer (nth 7 f)) (parse-integer (nth 8 f)) (parse-integer (nth 9 f)) (parse-integer (nth 10 f)) (parse-integer (nth 11 f)))
                out))))
    (nreverse out)))

(defun run-minimap2 (reference reads &key (preset "map-ont"))
  "Map READS to REFERENCE with minimap2 and return PAF-RECORDs."
  (parse-paf (tool-lines (run-tool "minimap2" (list "-x" preset reference reads)))))

;;; ---- samtools -------------------------------------------------------------------------------------------------------------------

(defun parse-samtools-depth (lines)
  "A list of (reference position depth) from `samtools depth` output (positions one-based)."
  (let ((out nil))
    (dolist (l lines) (let ((f (%split-on l #\Tab))) (when (= (length f) 3) (push (list (first f) (parse-integer (second f)) (parse-integer (third f))) out))))
    (nreverse out)))

(defun samtools-depth (bam &key region)
  "Per-position depth of the BAM file from `samtools depth` as (reference position depth) lists."
  (parse-samtools-depth (tool-lines (run-tool "samtools" (append (list "depth") (if region (list "-r" region) nil) (list bam))))))

(defun samtools-view (bam &key region)
  "SAM-RECORDs of BAM (optionally in REGION) through `samtools view`."
  (values (parse-sam (tool-lines (run-tool "samtools" (append (list "view") (list bam) (if region (list region) nil)))))))
