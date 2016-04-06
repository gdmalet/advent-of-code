;;;; Advent of code, day 6 problem 2.
;;;; Answer is 15343601.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *grid* (make-array '(1000 1000)
                                 :element-type 'integer
                                 :initial-element 0)
  "The grid of lights. The value is the element's brightness.")


(defun main ()
  (process-input-strings *input-file*)
  (format t "The total brightness is ~A~%"
          (count-lights)))

(defun count-lights ()
  "Count the lights in the grid that are on."
  (loop
     for y from 0 to (1- (array-dimension *grid* 1))
     sum
       (loop
          for x from 0 to (1- (array-dimension *grid* 0))
          sum (aref *grid* x y) into count
          finally (return count))
     into outer-count
     finally (return outer-count)))

(defun process-input-strings (from-file)
  "Process each line from the input file, one at a time."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)
    (loop
       for line = (read-line f nil)
       while line
       do
         (process-line line))))

(defun process-line (line)
  "Process one line of input through a pretty crude parser."
  (loop
     with start = 0
     with defun and indices = '()
     do
       (multiple-value-bind (token next)
           (get-token line start)

         (unless token
           (return (funcall defun indices)))

         (case token
           (TURN T)
           (THROUGH T)
           (ON
            (setf defun #'turn-on))
           (OFF
            (setf defun #'turn-off))
           (TOGGLE
            (setf defun #'toggle))
           (t
            (push token indices)))      ; creates a reversed list of indices.

         (setf start next))))

(defun turn-on (indices)
  "Increment the light brightness in the given range."
  (loop for y from (nth 2 indices) to (nth 0 indices) do
       (loop for x from (nth 3 indices) to (nth 1 indices) do
            (incf (aref *grid* x y)))))

(defun turn-off (indices)
  "Decrement the light brightness in the given range."
  (loop for y from (nth 2 indices) to (nth 0 indices) do
       (loop for x from (nth 3 indices) to (nth 1 indices) do
            (when (> (aref *grid* x y) 0)
              (decf (aref *grid* x y))))))

(defun toggle (indices)
  "Increment the light brightness by 2 in the given range."
  (loop for y from (nth 2 indices) to (nth 0 indices) do
       (loop for x from (nth 3 indices) to (nth 1 indices) do
            (incf (aref *grid* x y) 2))))


(defun get-token (line start)
  "Return the next token from the input line, and the next start position.
The token is nil when there's nothing left to read."

  ;; Make our own copy of the readtable, so we can change it
  (let ((*readtable* (copy-readtable)))

    ;; The lisp reader would normally barf on a raw comma, so instead
    ;; make it a delimiter which is otherwise ignored.
    (set-macro-character
     #\, (lambda (stream char)
           (declare (ignore char))
           (read stream t nil t)))

    (read-from-string line nil nil :start start)))
