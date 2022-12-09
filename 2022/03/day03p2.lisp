;;;; Advent of code, day 3 problem 2.
;;;; Answer is 2425.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun process-lines (one two three)
  (let* ((char-value (char-code (car (intersection (intersection (coerce one 'list)
																 (coerce two 'list))
												   (coerce three 'list))))))
    (if (> char-value 90)   ; a--z => 97--122
        (- char-value 96)   ; A--Z => 65--90
        (- char-value 38))))

(defun main ()
   (with-open-file
       (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

     (loop
       for one = (read-line f nil)
       for two = (read-line f nil)
       for three = (read-line f nil)
       while one
       summing (process-lines one two three) into total
       finally (return total))))
