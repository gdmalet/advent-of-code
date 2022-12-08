;;;; Advent of code, day 1 problem 2.
;;;; Answer is 209603.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun main ()
   (with-open-file
       (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

     (loop
	   with total = 0
	   with totals
        for cals = (read-line f nil)
        do
          (if (and cals (not (equal cals "")))
              (let ((value (read-from-string cals nil)))
                (incf total value))

              ;; a blank line, or end of file
              (progn
				(push total totals)
                (setf total 0)))
          
          ;; end of file
          (unless cals
			(let* ((final3 (last (sort totals '<) 3))
				  (final (apply '+ final3)))
				  (format t "Highest three values: ~A, total: ~A" final3 final)
            (return-from main final))))))
