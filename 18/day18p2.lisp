;;;; Advent of code, day 18 problem 2.
;;;; Answer is 924.

;;;; This is Conway's Game of Life.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *board-dimensions* '(100 . 100)
  "The size of our board.")

(defparameter *board* (make-array '(2 100 100) :element-type 'bit)
  "Two layer game board.")

(defconstant *neighbours*
  '((-1 . -1) (0 . -1) (1 . -1)
    (-1 .  0)          (1 .  0)
    (-1 .  1) (0 .  1) (1 .  1))
  "Indices to reach all a cell's neighbours, with origin in the bottom left corner.")

(defun main (&optional (steps 100) (board-dimensions *board-dimensions*))
  "Entry point to Conway's Game of Life."
  (unless (equal board-dimensions *board-dimensions*)
    (setf *board* (make-array
                   (list 2 (car board-dimensions) (cdr board-dimensions))
                   :element-type 'bit)))

  (fetch-input *input-file* board-dimensions)

  (loop
     repeat steps
     for depth = 1 then (- 1 depth)
     do
       (make-moves depth board-dimensions))

  (format t "There are ~D lights on after ~D steps.~%"
          (count-lights (logand steps 1) board-dimensions) steps))

;; A light which is on stays on when 2 or 3 neighbors are on, and
;; turns off otherwise.
;; A light which is off turns on if exactly 3 neighbors are on, and
;; stays off otherwise.
(defun make-moves (depth dimensions)
  "Advance one state on the given board; decided by looking at the
other board."
  (let ((x-size (car dimensions))
        (y-size (cdr dimensions)))

    (loop
       for y from 0 to (1- y-size)
       do
         (loop
            for x from 0 to (1- x-size)
            for count = (count-neighbours (- 1 depth) x y dimensions)
            do
              (ecase (aref *board* (- 1 depth) x y)
                (0                        ; it's currently off
                 (if (= count 3)
                     (setf (aref *board* depth x y) 1) ; turn it on
                     (setf (aref *board* depth x y) 0))) ; else stays off
                (1                        ; it's currently on
                 (if (or (= count 2)
                         (= count 3))
                     (setf (aref *board* depth x y) 1) ; it stays on
                     (setf (aref *board* depth x y) 0)))))) ; else turn it off

    ;; Finally, turn on the corner lights
    (setf (aref *board* depth 0 0) 1)
    (setf (aref *board* depth (1- x-size) 0) 1)
    (setf (aref *board* depth 0 (1- y-size)) 1)
    (setf (aref *board* depth (1- x-size) (1- y-size)) 1)))

(defun count-neighbours (depth x y dimensions)
  "Return how many neighbours are lit surrounding a given position."
  (loop
     with x-size = (car dimensions)
     with y-size = (cdr dimensions)
     for offset in *neighbours*
     for offset-x = (+ x (car offset))
     for offset-y = (+ y (cdr offset))
     when (and (>= offset-x 0)(< offset-x x-size)
               (>= offset-y 0)(< offset-y y-size))
     sum (aref *board* depth offset-x offset-y)
     into total
     finally
       (return total)))

(defun count-lights (depth dimensions)
  "Return how lights are on on the board."
  (loop
     with x-size = (car dimensions)
     with y-size = (cdr dimensions)
     for y from 0 to (1- y-size)
     sum (loop
            for x from 0 to (1- x-size)
            sum (aref *board* depth x y) into line-tot
            finally
              (return line-tot)) into board-tot
     finally
       (return board-tot)))

(defun fetch-input (from-file dimensions)
  "Fetch all the input from the file. Updates *board*."
  (let ((x-size (car dimensions))
        (y-size (cdr dimensions)))

    (with-open-file
        (f (make-pathname :name from-file)
           :direction :input
           :if-does-not-exist :error)

      (loop
         for y from 0 to (1- y-size)
         do
           (loop
              for x from 0 to (1- x-size)
              for char = (read-char f)
              do
                (ecase char
                  (#\. (setf (aref *board* 0 x y) 0))
                  (#\# (setf (aref *board* 0 x y) 1))))

           (read-char f)))              ; discard newline.

    ;; Finally, turn on the corner lights
    (setf (aref *board* 0 0 0) 1)
    (setf (aref *board* 0 (1- x-size) 0) 1)
    (setf (aref *board* 0 0 (1- y-size)) 1)
    (setf (aref *board* 0 (1- x-size) (1- y-size)) 1)))

(defun show-board (depth dimensions)
  "Show the current state of the board."
  (loop
     with x-size = (car dimensions)
     with y-size = (cdr dimensions)
     for y from 0 to (1- y-size)
     do
       (loop
          for x from 0 to (1- x-size)
          do
            (ecase (aref *board* depth x y)
              (0                        ; it's currently off
               (princ #\.))
              (1                        ; it's currently on
               (princ #\#))))
       (format t "~%"))
  (format t "~%"))
