;;;; Advent of code, day 9 problem 2.
;;;; Answer is 2661.

(defconstant +input-file+ "input.txt" "Where we read the goods.")

(defparameter *start-position* (cons 45 15)
"x y start position. Range is maxx 199, minx -43, maxy 263, miny -13
when starting at origin. This keeps things away from the edges.")

(defparameter *visited* (make-array '(300 300)
                                    :element-type '(integer 0 1)
                                    :initial-element 0))

(defparameter *heads* (make-array 10 :element-type 'cons))

;; If the head is ever two steps directly up, down, left, or
;; right from the tail, the tail must also move one step in
;; that direction so it remains close enough.
;; Otherwise, if the head and tail aren't touching and aren't in
;; the same row or column, the tail always moves one step
;; diagonally to keep up
(defun move-tail (head tail)
  (let ((apart-x (- (car head)
                    (car tail)))
        (apart-y (- (cdr head)
                    (cdr tail))))
    (when
        (or (> (abs apart-x) 1)         ; more than 1 place apart
            (> (abs apart-y) 1))

      (if (/= apart-x 0)
          (incf (car tail) (signum apart-x))) ; move the tail
      (if (/= apart-y 0)
          (incf (cdr tail) (signum apart-y))))))

(defun process-moves (steps count)

  (loop
     for dummy from count above 0
     do
       
     ;; move the first knot
       (incf (car (aref *heads* 0)) (car steps))
       (incf (cdr (aref *heads* 0)) (cdr steps))

     ;; move all the other knots
       (loop
          for c from 1 below (array-dimension *heads* 0)
          do
            (move-tail (aref *heads* (1- c)) (aref *heads* c)))

     ;; track position of last knot
       (setf (aref *visited*
                   (cdr (aref *heads* (1- (array-dimension *heads* 0))))
                   (car (aref *heads* (1- (array-dimension *heads* 0)))))
             1)

       (format t "step: ")
       (loop for i from 0 below 10 do (format t "~3d,~3a" (car (aref *heads* i)) (cdr (aref *heads* i))))       
       (format t "~%")))

(defun sum-array (ary)
  (loop
     for row upfrom 0 to (1- (array-dimension ary 0))
     sum (loop
            for col upfrom 0 to (1- (array-dimension ary 1))
            sum (aref ary row col))))

(defun main ()

  ;; must make sure contents is copies, not the same item, which :initial-element does.
  (loop
     for c from 0 below (array-dimension *heads* 0)
     do
       (setf (aref *heads* c) (copy-list *start-position*)))
  
  (setf (aref *visited*
              (cdr *start-position*)
              (car *start-position*)) 1)

  (with-open-file
      (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

      (loop
         for direction = (read f nil)
         for count = (read f nil) 
         while direction
         do
           (ecase (schar (symbol-name direction) 0)
             (#\R (process-moves (cons 1 0) count))
             (#\L (process-moves (cons -1 0) count))
             (#\U (process-moves (cons 0 1) count))
             (#\D (process-moves (cons 0 -1) count)))
         finally
           (return (sum-array *visited*)))))


