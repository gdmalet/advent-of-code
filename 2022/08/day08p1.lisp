;;;; Advent of code, day 8 problem 1.
;;;; Answer is 1820.

(defconstant +input-file+ "input.txt" "Where we read the goods.")

(defparameter *forest-size* '(99 99)) ;; '(99 99) '(5 5)
(defparameter *trees*   (make-array *forest-size* :element-type '(integer 0 9)))
(defparameter *visible* (make-array *forest-size* :element-type '(integer 0 1)
                                    :initial-element 0))

(defun check-heights ()
  ;; Mark corners visible
  (setf (aref *visible* 0 0) 1)
  (setf (aref *visible* 0 (1- (cadr *forest-size*))) 1)
  (setf (aref *visible* (1- (car *forest-size*)) 0) 1)
  (setf (aref *visible* (1- (car *forest-size*)) (1- (cadr *forest-size*))) 1)

  (flet ((check-tree (row col highest)
           (when (> (aref *trees* row col) highest)
             (setf highest (aref *trees* row col))
             (setf (aref *visible* row col) 1))
           highest))
           
    ;; Loop across rows
    (loop
       for row upfrom 1
       while (< row (1- (array-dimension *trees* 0)))
       do

       ;; left to right
         (loop
            with highest = -1
            for col upfrom 0
            while (and (< col (1- (array-dimension *trees* 1)))
                       (< highest 9))
            do
              (setf highest (check-tree row col highest)))

       ;; right to left
         (loop
            with highest = -1
            for col downfrom (1- (array-dimension *trees* 1))
            while (and (> col 0)
                       (< highest 9))
            do
              (setf highest (check-tree row col highest))))

  ;; Loop across columns
    (loop
       for col upfrom 1
       while (< col (1- (array-dimension *trees* 1)))
       do
       
       ;; top to bottom
         (loop
            with highest = -1
            for row upfrom 0
            while (and (< row (1- (array-dimension *trees* 1)))
                       (< highest 9))
            do
              (setf highest (check-tree row col highest)))

       ;; bottom to top
         (loop
            with highest = -1
            for row downfrom (1- (array-dimension *trees* 0))
            while (and (> row 0)
                       (< highest 9))
            do
              (setf highest (check-tree row col highest)))))
  *visible*)

(defun sum-array (ary)
  (loop
     for row upfrom 0 to (1- (array-dimension ary 0))
     sum (loop
            for col upfrom 0 to (1- (array-dimension ary 1))
            sum (aref ary row col))))

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
           (return (sum-array (check-heights))))))

           
