;;;; Advent of code, day 5 problem 2.
;;;; Answer is 69.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *vowels* '(#\a #\e #\i #\o #\u))
(defconstant *forbidden-strings* '("ab" "cd" "pq" "xy"))

(defun main ()
  (format t "There are ~A nice strings."
          (count-nice-strings *input-file*)))

(defun count-nice-strings (from-file)
  "Return a count of how many strings in the file are nice."

  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for str = (read-line f nil)
       while str
       count
         (string-is-nice-p str) into count
       finally
         (return count))))

(defun string-is-nice-p (str)
  "Returns T if a string is nice, else nil."
  (and
   (check-pairs str)
   (check-repeat-letter str)))

;; It contains a pair of any two letters that appears at least twice
;; in the string without overlapping, like xyxy (xy) or aabcdefgaa
;; (aa), but not like aaa (aa, but it overlaps).
(defun check-pairs (str)
  "Return T if the string contains the required non-overlapping pairs."

  (loop
     for i from 0 to (- (length str) 4)
     for pair = (concatenate 'string (list          ; yuk?
                                      (char str i)
                                      (char str (1+ i))))
     when (search pair str :start2 (+ 2 i))
       return T
     finally
       (return nil)))

;; It contains at least one letter which repeats with exactly one
;; letter between them, like xyx, abcdefeghi (efe), or even aaa.
(defun check-repeat-letter (str)
  "Returns T if the string contains one repeated letter, with one
letter between."

  (loop
     for i from 0 to (- (length str) 3)
     do
       (when (eql (char str i)
                  (char str (+ i 2)))
         (return T))
     finally
       (return nil)))
