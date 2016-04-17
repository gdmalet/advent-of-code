;;;; Advent of code, day 11 problem 1.
;;;; Answer for problem 1 is "hxbxxyzz".

(defconstant *puzzle-input* "hxbxwxba"
  "Puzzle input from the problem statement.")

(defconstant *forbidden-letters* '(#\i #\o #\l))

(defun main ()
  (format t "Next valid password after \"~A\" is \"~A\"~%"
          *puzzle-input*
          (loop
             for pass = (next-string (copy-seq *puzzle-input*))
                then (next-string pass)
             until
               (password-is-nice-p pass)
             finally
               (return pass))))

(defun next-string (str)
  "Return the next perhaps permissible password in the sequence."
  (loop
     for i downfrom (1- (length str)) to 0
     for next = (next-char (char str i))
     do
       ;; If next is a forbidden char, step whole string past it...
       ;; so "abcinnn" where n is anyghing is followed by "abcjaaa".
       (when next
         (when (find next *forbidden-letters*)
           (setf next (next-char next))
           (setf str
                 (concatenate
                  'string
                  (substring str 0 (1+ i))
                  (make-string (- (length str) i 1) :initial-element #\a))))
         (setf (char str i) next)
         (return))
       (setf (char str i) #\a))
  str)

(defun next-char (char &optional (wrap nil))
  "Return the next character in the alphabet, optionally wrapping to a
after z, else returning nil after z."
  (let ((next-code (1+ (char-code char))))
    (if (> next-code (char-code #\z))
      (if wrap
          #\a
          nil)
      (code-char next-code))))

(defun password-is-nice-p (str)
  "Returns T if a password is valid, else nil."
  (and
   (check-straight str)
   (check-dup-letter str)
   (check-forbidden str)))

;; Passwords must include one increasing straight of at least three
;; letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
;; letters; abd doesn't count
(defun check-straight (str)
  "Return T if the string contains required 3-letter straight."
  (loop
     for i from 0 to (- (length str) 3)
     do
       (when (and (equal (char str (+ i 1))
                         (next-char (char str i)))
                  (equal (char str (+ i 2))
                         (next-char (char str (+ i 1)))))
         (return t))
     finally (return nil)))

;; Passwords may not contain the letters i, o, or l, as these letters
;; can be mistaken for other characters and are therefore confusing.
(defun check-forbidden (str)
  "Returns T if the string does not contain a forbidden letter, else nil."
  (loop
     for f in *forbidden-letters*
     do
       (if (find f str)
           (return nil))
     finally
       (return t)))

;; Passwords must contain at least two different, non-overlapping
;; pairs of letters, like aa, bb, or zz.
(defun check-dup-letter (str)
  "Returns T if the string twice contains one letter that appears
twice in a row."
  (loop
     with count = 0
     for i from 0 to (- (length str) 2)
     do
       (when (eql (char str i)
                  (char str (1+ i)))
         (if (> count 0)
             (return t))
         (incf count)
         (incf i))
     finally
       (return nil)))
