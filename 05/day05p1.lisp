;;;; Advent of code, day 5 problem 1.
;;;; Answer is 238.

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
   (check-vowels str)
   (check-dup-letter str)
   (check-forbidden str)))

;; "It contains at least three vowels (aeiou only), like aei, xazegov,
;; or aeiouaeiouaeiou."
(defun check-vowels (str)
  "Return T if the string contains at lease 3 vowels, else nil."

  (loop
     for v in *vowels*
     summing
       (count v str) into count
     when (>= count 3) return T
     finally (return nil)))

;; It contains at least one letter that appears twice in a row, like
;; xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
(defun check-dup-letter (str)
  "Returns T if the string contains one letter that appears twice in a row."

  (loop
     for i from 0 to (- (length str) 2)
     do
       (when (eql (char str i)
                  (char str (1+ i)))
         (return-from check-dup-letter T)))

  nil)

;; It does not contain the strings ab, cd, pq, or xy, even if they are
;; part of one of the other requirements.
(defun check-forbidden (str)
  "Returns T if the string does not contain a forbidden string, else nil."

  (loop
     for f in *forbidden-strings*
     do
       (if (search f str)
           (return-from check-forbidden nil)))
  t)
