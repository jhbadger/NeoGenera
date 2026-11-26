;;;; 63-unicode.lisp -- Unicode properties for UTF-8 text: display width, case mapping, word characters.
;;;;
;;;; Strings are UTF-8 byte strings; these functions work on code points (integers) and on such strings.
;;;; Widths follow the usual terminal convention (wcwidth): combining marks and other format characters take
;;;; no column, East Asian wide and fullwidth characters and most emoji take two.  Case mapping covers ASCII,
;;;; Latin-1, Latin Extended-A, Latin Extended Additional (Vietnamese), Greek, Cyrillic, Armenian and the
;;;; fullwidth Latin letters, one code point to one code point (so ß stays ß).

(in-package :cl)

(defun %ranges (list) (coerce list 'vector))

(defparameter *zero-width-ranges*
  (%ranges '(#x0300 #x036F #x0483 #x0489 #x0591 #x05BD #x05BF #x05BF #x05C1 #x05C2 #x05C4 #x05C5 #x05C7 #x05C7
             #x0610 #x061A #x064B #x065F #x0670 #x0670 #x06D6 #x06DC #x06DF #x06E4 #x06E7 #x06E8 #x06EA #x06ED
             #x0900 #x0902 #x093A #x093A #x093C #x093C #x0941 #x0948 #x094D #x094D #x0951 #x0957
             #x0E31 #x0E31 #x0E34 #x0E3A #x0E47 #x0E4E #x1160 #x11FF #x1AB0 #x1AFF #x1DC0 #x1DFF
             #x200B #x200F #x202A #x202E #x2060 #x2064 #x20D0 #x20FF #x302A #x302D #x3099 #x309A
             #xFE00 #xFE0F #xFE20 #xFE2F #xFEFF #xFEFF #x1F3FB #x1F3FF #xE0001 #xE0001 #xE0020 #xE007F #xE0100 #xE01EF))
  "Code point ranges (low high pairs) that take no column: combining marks, joiners, variation selectors.")

(defparameter *wide-ranges*
  (%ranges '(#x1100 #x115F #x231A #x231B #x2329 #x232A #x23E9 #x23EC #x23F0 #x23F0 #x23F3 #x23F3 #x25FD #x25FE
             #x2614 #x2615 #x2648 #x2653 #x267F #x267F #x2693 #x2693 #x26A1 #x26A1 #x26AA #x26AB #x26BD #x26BE
             #x26C4 #x26C5 #x26CE #x26CE #x26D4 #x26D4 #x26EA #x26EA #x26F2 #x26F3 #x26F5 #x26F5 #x26FA #x26FA
             #x26FD #x26FD #x2705 #x2705 #x270A #x270B #x2728 #x2728 #x274C #x274C #x274E #x274E #x2753 #x2755
             #x2757 #x2757 #x2795 #x2797 #x27B0 #x27B0 #x27BF #x27BF #x2B1B #x2B1C #x2B50 #x2B50 #x2B55 #x2B55
             #x2E80 #x3029 #x302E #x303E #x3041 #x3098 #x309B #x33FF #x3400 #x4DBF #x4E00 #x9FFF #xA000 #xA4CF
             #xA960 #xA97F #xAC00 #xD7A3 #xF900 #xFAFF #xFE10 #xFE19 #xFE30 #xFE6F #xFF00 #xFF60 #xFFE0 #xFFE6
             #x16FE0 #x16FE4 #x17000 #x18AFF #x1B000 #x1B2FF #x1F004 #x1F004 #x1F0CF #x1F0CF #x1F18E #x1F18E
             #x1F191 #x1F19A #x1F200 #x1F251 #x1F300 #x1F320 #x1F32D #x1F335 #x1F337 #x1F37C #x1F37E #x1F393
             #x1F3A0 #x1F3CA #x1F3CF #x1F3D3 #x1F3E0 #x1F3F0 #x1F3F4 #x1F3F4 #x1F3F8 #x1F3FA #x1F400 #x1F43E
             #x1F440 #x1F440 #x1F442 #x1F4FC #x1F4FF #x1F53D #x1F54B #x1F54E #x1F550 #x1F567 #x1F57A #x1F57A
             #x1F595 #x1F596 #x1F5A4 #x1F5A4 #x1F5FB #x1F64F #x1F680 #x1F6C5 #x1F6CC #x1F6CC #x1F6D0 #x1F6D2
             #x1F6D5 #x1F6D7 #x1F6EB #x1F6EC #x1F6F4 #x1F6FC #x1F7E0 #x1F7EB #x1F90C #x1F93A #x1F93C #x1F945
             #x1F947 #x1F9FF #x1FA70 #x1FAFF #x20000 #x2FFFD #x30000 #x3FFFD))
  "Code point ranges that take two columns: East Asian wide and fullwidth characters, and emoji.")

(defparameter *non-word-ranges*
  (%ranges '(#x00A0 #x00BF #x00D7 #x00D7 #x00F7 #x00F7 #x02C2 #x02C5 #x02D2 #x02DF #x037E #x037E #x0387 #x0387
             #x055A #x055F #x0589 #x058A #x060C #x060D #x061B #x061F #x066A #x066D #x06D4 #x06D4
             #x2000 #x2BFF #x2E00 #x2E7F #x3000 #x3003 #x3008 #x3020 #x3030 #x3030 #xFD3E #xFD3F #xFE10 #xFE19
             #xFE30 #xFE6F #xFF00 #xFF0F #xFF1A #xFF20 #xFF3B #xFF40 #xFF5B #xFF65 #xFFE0 #xFFEE
             #x1F000 #x1FAFF))
  "Code point ranges of punctuation, symbols, spaces and emoji: not part of words.")

(defun %in-ranges (code ranges)
  (let ((lo 0) (hi (- (floor (length ranges) 2) 1)))
    (loop
      (when (> lo hi) (return nil))
      (let* ((mid (floor (+ lo hi) 2)) (a (aref ranges (* 2 mid))) (b (aref ranges (+ (* 2 mid) 1))))
        (cond ((< code a) (setq hi (- mid 1)))
              ((> code b) (setq lo (+ mid 1)))
              (t (return t)))))))

(defun char-display-width (code)
  "Columns a terminal gives code point CODE: 0 for combining marks and other zero-width characters, 2 for East
Asian wide characters and emoji, otherwise 1."
  (cond ((< code #x300) 1)
        ((%in-ranges code *zero-width-ranges*) 0)
        ((< code #x1100) 1)
        ((%in-ranges code *wide-ranges*) 2)
        (t 1)))

(defun utf8-char-at (string index)
  "The code point of the UTF-8 character that starts at byte INDEX of STRING, and the index after it."
  (let ((b (char-code (char string index))))
    (if (< b 128) (values b (+ index 1)) (%utf8-decode string index))))

(defun utf8-codes (string)
  "The code points of the UTF-8 STRING, as a list."
  (let ((acc nil) (i 0) (n (length string)))
    (loop (when (>= i n) (return))
          (multiple-value-bind (code next) (utf8-char-at string i) (setq acc (cons code acc) i next)))
    (nreverse acc)))

(defun utf8-from-codes (codes)
  "A UTF-8 string of the code points CODES."
  (apply #'%string-concat (mapcar (lambda (c) (if (< c 128) (string (code-char c)) (%utf8-encode c))) codes)))

(defun string-display-width (string &optional (start 0) end)
  "Columns the UTF-8 STRING (from byte START to END) takes on a terminal."
  (let ((i start) (n (or end (length string))) (w 0))
    (loop (when (>= i n) (return w))
          (let ((b (char-code (char string i))))
            (if (< b 128)
                (setq w (+ w 1) i (+ i 1))
                (multiple-value-bind (code next) (%utf8-decode string i)
                  (setq w (+ w (char-display-width code)) i next)))))))

(defun %latin-a-up (code)
  (cond ((= code #x131) #x49) ((= code #x17F) #x53)
        ((or (<= #x100 code #x137) (<= #x14A code #x177)) (if (oddp code) (- code 1) code))
        ((or (<= #x139 code #x148) (<= #x179 code #x17E)) (if (evenp code) (- code 1) code))
        (t code)))

(defun %latin-a-down (code)
  (cond ((= code #x130) #x69) ((= code #x178) #xFF)
        ((or (<= #x100 code #x137) (<= #x14A code #x177)) (if (evenp code) (+ code 1) code))
        ((or (<= #x139 code #x148) (<= #x179 code #x17E)) (if (oddp code) (+ code 1) code))
        (t code)))

(defun unicode-upcase (code)
  "The upper-case code point for CODE (CODE itself if it has none)."
  (cond ((<= 97 code 122) (- code 32))
        ((< code 128) code)
        ((<= #xE0 code #xFE) (if (= code #xF7) code (- code 32)))
        ((= code #xFF) #x178)
        ((= code #xB5) #x39C)
        ((< code #x100) code)
        ((<= code #x17F) (%latin-a-up code))
        ((<= #x3B1 code #x3C9) (if (= code #x3C2) #x3A3 (- code 32)))
        ((<= #x3CA code #x3CB) (- code 32))
        ((= code #x3AC) #x386) ((<= #x3AD code #x3AF) (- code 37)) ((= code #x3CC) #x38C) ((<= #x3CD code #x3CE) (- code 63))
        ((<= #x430 code #x44F) (- code 32))
        ((<= #x450 code #x45F) (- code 80))
        ((or (<= #x460 code #x481) (<= #x48A code #x4BF) (<= #x4D0 code #x52F)) (if (oddp code) (- code 1) code))
        ((<= #x4C1 code #x4CE) (if (evenp code) (- code 1) code))
        ((= code #x4CF) #x4C0)
        ((<= #x561 code #x586) (- code 48))
        ((<= #x1E00 code #x1EFF) (if (and (oddp code) (not (<= #x1E96 code #x1E9F))) (- code 1) code))
        ((<= #xFF41 code #xFF5A) (- code 32))
        (t code)))

(defun unicode-downcase (code)
  "The lower-case code point for CODE (CODE itself if it has none)."
  (cond ((<= 65 code 90) (+ code 32))
        ((< code 128) code)
        ((<= #xC0 code #xDE) (if (= code #xD7) code (+ code 32)))
        ((< code #x100) code)
        ((<= code #x17F) (%latin-a-down code))
        ((<= #x391 code #x3A9) (if (= code #x3A2) code (+ code 32)))
        ((<= #x3AA code #x3AB) (+ code 32))
        ((= code #x386) #x3AC) ((<= #x388 code #x38A) (+ code 37)) ((= code #x38C) #x3CC) ((<= #x38E code #x38F) (+ code 63))
        ((<= #x410 code #x42F) (+ code 32))
        ((<= #x400 code #x40F) (+ code 80))
        ((or (<= #x460 code #x481) (<= #x48A code #x4BF) (<= #x4D0 code #x52F)) (if (evenp code) (+ code 1) code))
        ((<= #x4C1 code #x4CE) (if (oddp code) (+ code 1) code))
        ((= code #x4C0) #x4CF)
        ((<= #x531 code #x556) (+ code 48))
        ((<= #x1E00 code #x1EFF) (if (and (evenp code) (not (<= #x1E96 code #x1E9F))) (+ code 1) code))
        ((<= #xFF21 code #xFF3A) (+ code 32))
        (t code)))

(defun unicode-fold (code)
  "CODE folded for case-insensitive comparison."
  (let ((d (unicode-downcase code)))
    (cond ((= d #x3C2) #x3C3) ((= d #x17F) #x73) (t d))))

(defun unicode-upper-case-p (code) "True if CODE is an upper-case letter." (/= code (unicode-downcase code)))
(defun unicode-lower-case-p (code) "True if CODE is a lower-case letter." (/= code (unicode-upcase code)))

(defun unicode-word-char-p (code)
  "True if CODE is part of a word: a letter, digit or underscore, or any other character that is not
punctuation, a symbol, a space or an emoji."
  (if (< code 128)
      (or (<= 48 code 57) (<= 65 code 90) (<= 97 code 122) (= code 95))
      (not (%in-ranges code *non-word-ranges*))))

(defun utf8-upcase (string) "STRING (UTF-8) in upper case." (utf8-from-codes (mapcar #'unicode-upcase (utf8-codes string))))
(defun utf8-downcase (string) "STRING (UTF-8) in lower case." (utf8-from-codes (mapcar #'unicode-downcase (utf8-codes string))))

(defun utf8-capitalize (string)
  "STRING (UTF-8) with the first letter of each word in upper case and the rest in lower case."
  (let ((in-word nil) (acc nil))
    (dolist (c (utf8-codes string))
      (let ((w (unicode-word-char-p c)))
        (setq acc (cons (cond ((not w) c) (in-word (unicode-downcase c)) (t (unicode-upcase c))) acc))
        (setq in-word w)))
    (utf8-from-codes (nreverse acc))))
