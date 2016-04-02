;;;; Advent of code, day 1 problem 1.
;;;; Answer is 138.

(defconstant *input-file* "input.txt"
  "Where we read to directions.")

(defun main()
  (format t "Santa ends on floor ~A.~%" (count-floors *input-file*)))

(defun count-floors(from-file)
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with floor = 0
       for brace = (read-char f nil)
       do
         (ecase brace
           (#\( (incf floor))
           (#\) (decf floor))
           ((nil) (return-from count-floors floor))))))
