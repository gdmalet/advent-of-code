;;;; Advent of code, day 1 problem 1.
;;;; Answer is 1771.

(defconstant *input-file* "input.txt"
  "Where we read the directions.")

(defun main()
  (let ((char-num (find-basement *input-file*)))
    (case char-num
      ((nil) (format t "Santa didn't enter the basement.~%"))
      (t (format t "Santa enters the basement on char ~A.~%" char-num)))))
        

(defun find-basement(from-file)
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with floor = 0
       with char-count = 0
       for brace = (read-char f nil)
       do
         (incf char-count)
         (ecase brace
           (#\( (incf floor))
           (#\) (decf floor))
           ((nil) (return-from find-basement nil)))
         when (= floor -1) return char-count)))
