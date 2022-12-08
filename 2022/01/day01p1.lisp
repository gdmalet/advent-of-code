;;;; Advent of code, day 1 problem 1.
;;;; Answer is 71506.

(defconstant *input-file* "input.txt"
  "Where we read the goods.")

(defun main ()
   (with-open-file
       (f (make-pathname :name *input-file*)
         :direction :input
         :if-does-not-exist :error)

     (loop
        with max = 0
        with max-num
        with total = 0
        with num = 1

        for cals = (read-line f nil)
        do
          (if (and cals (not (equal cals "")))
              (let ((value (read-from-string cals nil)))
                (incf total value))

              ;; a blank line, or end of file
              (progn
                (when (> total max)
                  (format t "max ~A at ~A~%" total num)
                  (setf max total
                      max-num num))
                (setf num (1+ num)
                      total 0)))
          
          ;;; end of file
          (unless cals
            (format t "max is ~A from number ~A~%" max max-num)
            (return-from main (list max max-num))))))


 
