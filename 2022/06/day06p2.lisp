;;;; Advent of code, day 6 problem 2.
;;;; Answer is 3645.

(defconstant +input-file+ "input.txt" "Where we read the goods.")
(defconstant +message-length+ 14 "How many unique chars to scan for.")

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
         (when (> (length shift-register) +message-length+)
           (pop shift-register)
           (when (= +message-length+ (length (remove-duplicates shift-register)))
             (format t "Got ~A unique chars in ~A at ~A~%"
                     +message-length+ shift-register count)
             (return-from main (values count shift-register)))))))
