;;;; Advent of code, day 6 problem 1.
;;;; Answer is 1080.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun main ()
  (with-open-file
      (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with shift-register = '()
       for count from 1
       for char = (read-char f nil)
       do
         (format t "char ~A, register ~A, count ~A~%" char shift-register count)
         (setf shift-register (append shift-register (list char)))
         (when (> (length shift-register) 4)
           (pop shift-register)
           (when (= 4 (length (remove-duplicates shift-register)))
             (format t "Got 4 unique chars in ~A at ~A~%" shift-register count)
             (return-from main (values count shift-register)))))))
