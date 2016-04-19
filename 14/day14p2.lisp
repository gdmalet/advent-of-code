;;;; Advent of code, day 14 problem 2.
;;;; Answer is 1256 points.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *race-time* 2503
  "How long the race is.")

(defun main ()
  (format t "The winning points score of a ~D second race is ~D.~%"
          *race-time* (run-the-races)))

;; Step over each second of a race, and each step see who is in the
;; lead (maximum distance in the race-distances array), and for each
;; reindeer that has covered that distance give them one bonus point
;; (in the bonuses array). Finally return the highest value from
;; bonuses.
(defun run-the-races (&optional (longest-race-time *race-time*))
  "Run all possible steps of a race, and return the highest bonus value."
  (loop
     with stats = (fetch-input *input-file*)
     with bonuses = (make-array (length stats) :initial-element 0)
     with race-distances = (make-array (length stats))
     for race-time from 1 to longest-race-time
     do
       ;; run each reindeer for this time
       (loop
          for i from 0 to (1- (length stats))
          do
            (setf (aref race-distances i) 
                  (race-distance (nth i stats) race-time)))

       ;; find the best distance for this time
       (let ((best-time
              (loop
                 for i from 0 to (1- (length stats))
                 maximize (aref race-distances i) into max
                 finally
                   (return max))))

         ;; hand out bonuses for anyone that matched the best distance
         (loop
            for i from 0 to (1- (length stats))
            do
              (if (eql (aref race-distances i) best-time)
                  (incf (aref bonuses i))))

         (format t "~D: bonuses for ~D in ~A --> ~A~%"
                 race-time best-time race-distances bonuses))

     ;; Find the highest bonus value
     finally
       (return
         (do* ((best 0)
               (i 0 (1+ i)))
              ((= i (length stats)) best)
           (if (> (aref bonuses i) best)
               (setf best (aref bonuses i)))))))

(defun winning-distance (race-time all-stats)
  "Find the winning distance for a race."
  (loop
     with best = 0
     for stats in all-stats
     for reindeer = (race-distance stats race-time)
     do
       (format t "pondering ~D in ~D secs, ~A~%" reindeer race-time stats)
       (if (> reindeer best)
           (setf best reindeer))
     finally
       (return best)))

(defun race-distance (stats race-time)
  "Return how far the given reindeer would race in the given time."
  (let* ((speed (nth 1 stats))
        (fast-time (nth 2 stats))
        (rest-time (nth 3 stats))
        (chunk-time (+ fast-time rest-time)))

    (multiple-value-bind (quot rem)
        (floor race-time chunk-time)
      (+ (* speed fast-time quot)
         (* speed (min rem fast-time))))))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns a list of stats for each
reindeer, with stats being name, speed, speed time, rest time."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (fetch-line f)
       while line
       collect line)))

;;; This gaily assumes all input is in the format:
;;; Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
(defun fetch-line (f)
  "Fetch a line of input from stream f. Returns list of name, speed,
time and rest time."

    ;; The lisp reader would do silly things on a raw comma so make it
    ;; a delimiter which is otherwise ignored. Ignore dots too.
  (let ((*readtable* (copy-readtable)))
    (flet ((ignore-char (stream char)
             (declare (ignore char))
             (read stream nil nil nil)))
      (set-macro-character #\, #'ignore-char)
      (set-macro-character #\. #'ignore-char)

      (loop
         repeat 15
         for word = (read f nil)
         unless word return nil
         collect word into words
         finally
           (return
             (list
              (car words)
              (nth 3 words)
              (nth 6 words)
              (nth 13 words)))))))
