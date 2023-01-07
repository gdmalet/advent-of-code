;;;; Advent of code, day 9 problem 1.
;;;; Answer is 6284.

(defconstant +input-file+ "input.txt" "Where we read the goods.")

(defparameter *start-position* (cons 45 15)
"x y start position. Range is maxx 199, minx -43, maxy 263, miny -13
when starting at origin. This keeps things away from the edges.")

(defparameter *visited* (make-array '(300 300)
                                    :element-type '(integer 0 1)
                                    :initial-element 0))

(defun process-moves (head tail steps count)
  (loop
     for c from count above 0
     do
       (incf (car head) (car steps))
       (incf (cdr head) (cdr steps))

     ;; If the head is ever two steps directly up, down, left, or
     ;; right from the tail, the tail must also move one step in
     ;; that direction so it remains close enough.
     ;; Otherwise, if the head and tail aren't touching and aren't in
     ;; the same row or column, the tail always moves one step
     ;; diagonally to keep up
       
       (when
           (or (> (abs                      ; more than 1 place apart
                   (- (car head) (car tail)))
                  1)
               (> (abs
                   (- (cdr head) (cdr tail)))
                  1))

         (incf (car tail) (car steps))  ; move the tail
         (incf (cdr tail) (cdr steps))

         (when
             (not                               ; diagonal move reqd?
              (or (eql (car head) (car tail))   ; same row
                  (eql (cdr head) (cdr tail)))) ; same col
           (if (eql (car steps) 0)              ; y increment
               (setf (car tail) (car head))     ; move x
               (setf (cdr tail) (cdr head))))   ; else move y

         (setf (aref *visited* (cdr tail) (car tail)) 1))
     
       (format t " step: head ~A, tail ~A~%" head tail)))

(defun sum-array (ary)
  (loop
     for row upfrom 0 to (1- (array-dimension ary 0))
     sum (loop
            for col upfrom 0 to (1- (array-dimension ary 1))
            sum (aref ary row col))))

(defun main ()

  (setf (aref *visited*
              (cdr *start-position*)
              (car *start-position*)) 1) ; set start position as visited

  (with-open-file
      (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

      (loop
         with head = *start-position*
         with tail = (cons (car *start-position*) (cdr *start-position*)) ; force copy
         for direction = (read f nil)
         for count = (read f nil) 
         while direction
         do
           (ecase (schar (symbol-name direction) 0)
             (#\R (process-moves head tail (cons 1 0) count))
             (#\L (process-moves head tail (cons -1 0) count))
             (#\U (process-moves head tail (cons 0 1) count))
             (#\D (process-moves head tail (cons 0 -1) count)))
         finally
           (return (sum-array *visited*)))))


