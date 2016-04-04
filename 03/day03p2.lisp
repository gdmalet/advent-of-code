;;;; Advent of code, day 3 problem 2.
;;;; Answer is 2360.
;;;;
;;;; We need to represent "an infinite two-dimensional grid", so while
;;;; an NxN array is tempting, in theory we could run out of
;;;; memory. So trust that lisp's implementation of associative arrays
;;;; is more memory efficient, and use them to track locations.

(defparameter *moves* (make-hash-table :size 8192 :test 'EQUAL)
  "Used to track how many times we've visited each location.")

(defconstant *input-file* "input.txt"
  "Where we read the directions.")

(defun main ()
  (setf (gethash (cons 0 0) *moves*) 2) ; start point is visited by each

  (format t "~A houses have received at least one present.~%"
          (hash-table-count (make-moves *input-file*))))

(defun make-moves(from-file)
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with locations = (list '(0 . 0) '(0 . 0)) ; Santa & the robot's location
       for index = 0 then (- 1 index)  ; alternate index between 0 & 1
       for move = (read-char f nil)
       do
         (ecase move
           (#\< (decf (car (nth index locations))))
           (#\> (incf (car (nth index locations))))
           (#\^ (incf (cdr (nth index locations))))
           (#\v (decf (cdr (nth index locations))))
           ((nil) (return-from make-moves *moves*)))

         ;; Increment the number of times we've visited this location.
         ;; If this is the first visit, default gethash to returning 0.
         (incf (gethash (cons (car (nth index locations))
                              (cdr (nth index locations)))
                        *moves* 0)))))
