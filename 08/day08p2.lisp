;;;; Advent of code, day 8 problem 2.
;;;; Answer is 2046.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defun main ()
  "Process all the strings, and do the math."
  (format t "Total encoded string length minus original length is ~D.~%"
          (process-input-strings *input-file*)))

(defun process-input-strings (from-file)
  "Process each line from the input file, one at a time."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (read-line f nil)
       while line
       sum
         (- (length (escape-string line))
            (length line))
       into diff
       finally
         (return diff))))

(defun escape-string (line)
  "Return the properly escaped input string."
  ;; Step through the existing string, and build up a new one to return."
  ;; Begin & end with enclosing quotes.
  (loop
     with chars = '(#\")
     for i from 0 to (1- (length line))
     do
       ;; Only need to escape backslash & quote, by prepending a backslash.
       (case (char line i)
         (#\\
          (push #\\ chars))
         (#\"
          (push #\\ chars)))
       (push (char line i) chars)
     finally
       (return
         (coerce (nreverse (push #\" chars)) 'string))))
