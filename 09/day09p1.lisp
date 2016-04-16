;;;; Advent of code, day 9 problem 1.
;;;; Answer is 117.

;;;; This is really a brute force solution to the Travelling Salesman
;;;; Problem. A hash table is built from the input, containing each
;;;; possible starting town as a key, and a list of values of the next
;;;; town & distance from that key. Then brute-force generate each
;;;; possible route in a recursive call, and check the distance when
;;;; we bottom out.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *input* (make-hash-table :test 'EQUAL)
  "Processed input from the file above.")

(defparameter *shortest-distance* (cons most-positive-fixnum nil)
  "The shortest distance we have so far.")

(defun main ()
  "Do all initialisation, then find a solution."

  ;; Load input into a hash table for easier processing
  (fetch-input *input-file*)

  ;; Loop over each possible starting point
  (loop
     for key being the hash-keys in *input*
     do
       (start-at (gethash key *input*)  ; next hops from key town
                 (list key)             ; route so far
                 0                      ; distance so far
                 (1- (hash-table-count *input*)))) ; how deep we must go

  (format t "Shortest route: ~D -> ~A~%"
          (car *shortest-distance*)
          (cdr *shortest-distance*)))
         

(defun start-at (destinations route distance depth)

  (when (= depth 0)                  ; bottomed out
    (format t "route: ~D: ~A~%" distance (nreverse route))
    (when (< distance (car *shortest-distance*))
      (format t "best: ~D: ~A~%"  distance (nreverse route))
      (setf *shortest-distance* (cons distance (nreverse route))))
    (return-from start-at))

  ;; Find a town we haven't visited yet, add it to the route, and add
  ;; the distance
  (loop
     for local-route = (copy-list route)
     for dest in destinations
     do
       (unless (find (car dest) route :test #'equal)
         (start-at (gethash (car dest) *input*)
                   (push (car dest) local-route)
                   (+ distance (cdr dest))
                   (1- depth)))))


(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns a hash table with each
key being a town, and the values a list of dotted pairs of destination
and distance. Updates *input*."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for town-line = (fetch-line f)
       while town-line
       do
         (push (cons (nth 1 town-line) (nth 2 town-line))
               (gethash (nth 0 town-line) *input* '()))
         (push (cons (nth 0 town-line) (nth 2 town-line))
               (gethash (nth 1 town-line) *input* '()))
       finally
         (return *input*))))

;;; This gaily assumes all input grouped by from town, and in the format:
;;; Faerun to Tristram = 65, which implies a distance in each direction.
(defun fetch-line (f)
  "Fetch a line of input from stream f. Returns triplet of from & to
towns, and distance."
  (loop
     with towns = '()
     for i from 1 to 5
     for town = (read f nil)
     while town
     do
       (when (oddp i)
         (push town towns))
     finally (return (nreverse towns))))
