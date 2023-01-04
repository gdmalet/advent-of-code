;;;; Advent of code, day 8 problem 2.
;;;; Answer is 385112.

(defconstant +input-file+ "input.txt" "Where we read the goods.")

(defparameter *forest-size* '(99 99)) ;; '(99 99) '(5 5)
(defparameter *trees*   (make-array *forest-size* :element-type '(integer 0 9)))

(defun check-heights ()
  (flet ((check-directions (row col direction)
           (loop
              with height = (aref *trees* row col)
              with y = (+ row (car direction))
              with x = (+ col (cadr direction))
              for distance upfrom 1
              while (and (> x 0)
                         (> y 0)
                         (< x (1- (array-dimension *trees* 0)))
                         (< y (1- (array-dimension *trees* 1)))
                         (< (aref *trees* y x) height))
              do
                (incf x (cadr direction))
                (incf y (car direction))
              finally
                (return distance))))
           
    ;; Loop across rows
    (loop
       for row upfrom 1
       while (< row (1- (array-dimension *trees* 0)))
       collect                          ;; lists of 4 values, one list for each row
         (loop
            for col upfrom 1
            while (< col (1- (array-dimension *trees* 1)))
            collect
              (reduce #'*         ;; 4 values, one for each direction
                      ;; check each direction from here
                      (loop
                         for direction in '((0 1) (0 -1) (1 0) (-1 0))
                         collect 
                           (check-directions row col direction)))))))

(defun main ()
  (with-open-file
      (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

      (loop
         for line = (read-line f nil)
         for row upfrom 0 
         while line
         do
           (loop
              for col upfrom 0
              while (< col (length line))
              do
                (setf (aref *trees* row col)
                      (parse-integer line :start col :end (1+ col))))
         finally
           (return (apply #'max
                          (loop for elt in (check-heights)
                             collect (apply #'max elt)))))))
