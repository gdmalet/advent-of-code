;;;; Advent of code, day 2 problem 2.
;;;; Answer is 2592.
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
  (setf (gethash (cons 0 0) *moves*) 1) ; start point is visited

  (format t "~A houses have been visited at least once.~%"
          (hash-table-count (make-moves *input-file*))))

(defun make-moves(from-file)
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       with x = 0
       with y = 0
       for move = (read-char f nil)
       do
         (ecase move
           (#\< (decf x))
           (#\> (incf x))
           (#\^ (incf y))
           (#\v (decf y))
           ((nil) (return-from make-moves *moves*)))

         ;; Increment the number of times we've visited this location.
         ;; If this is the first visit, default gethash to returning 0.
         (incf (gethash (cons x y) *moves* 0)))))
