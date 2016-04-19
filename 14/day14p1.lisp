;;;; Advent of code, day 14 problem 1.
;;;; Answer is 2660 km.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *race-time* 2503
  "How long the race is.")

(defun main ()
  (format t "The winning distance of a ~D second race is ~D km.~%"
          *race-time* (winning-distance)))

(defun winning-distance ()
  "Find the winning distance for a race."
  (loop
     with best = 0
     for stats in (fetch-input *input-file*)
     for reindeer = (race-distance stats *race-time*)
     do
       (format t "pondering ~D ~A~%" reindeer stats)
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
