;;;; Advent of code, day 3 problem 1.
;;;; Answer is 8053.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun process-line (line)
  (let* ((len (/ (length line) 2))
		 (left (subseq line 0 len))
         (right (subseq line len))
         (char-value (char-code (car (intersection (coerce left 'list)
                                                   (coerce right 'list))))))
    (if (> char-value 90)   ; a--z => 97--122
        (- char-value 96)   ; A--Z => 65--90
        (- char-value 38))))

(defun main ()
   (with-open-file
       (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

     (loop
       for line = (read-line f nil)
       while line
       summing (process-line line) into total
       finally (return total))))
