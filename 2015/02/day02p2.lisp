;;;; Advent of code, day 2 problem 2.
;;;; Answer is 3812909.

(defconstant *input-file* "input.txt"
  "Where we read the dimensions.")

(defun main ()
  (format t "Total length of ribbon is ~A units~%"
          (measure-ribbon *input-file*)))

(defun measure-ribbon (from-file)
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with total = 0
       for dimensions = (read-line f nil)
       do
         (unless dimensions
           (return-from measure-ribbon total))

         (let ((sides (read-values dimensions)))
           (incf total
                 (+
                  (* 2 (+ (nth 0 sides) (nth 1 sides)))
                  (* (nth 0 sides) (nth 1 sides) (nth 2 sides))))))))

;; Returns a cons of the three integer parts of a string such as "11x2x13".
;; Values are sorted into ascending order.
(defun read-values (string)
  (loop
     with start = 0
     with values = nil
     do
       (multiple-value-bind (value end)
           (parse-integer string
                          :junk-allowed t
                          :start start)
         (push value values)
         (when (= end (length string))
           (return-from read-values (sort values #'<)))
         (setf start (1+ end)))))
