;;;; Advent of code, day 10 problem 1.
;;;; Answer is 360154.

(defconstant *puzzle-input* "1113122113"
  "Puzzle input from the problem statement.")

(defun main (&optional (times 40))
  "Repeat look-and-say 40 times for given input string."
  (loop
     for str = *puzzle-input* then (look-and-say str)
     for i from 0 to times
     do
       (format t "~2D (~d) ~A~%" i (length str)
               (if (< (length str) 100)
                   str
                   (concatenate 'string (substring str 0 97) "...")))))

(defun look-and-say (str)
  "Return 'look and say' string for given input string."
  (loop
     for start = 0 then (+ start count)
     for count = (count-first-char str start)
     while (> count 0)
     collect count into result
       collect (char str start) into result
     finally
       (return (with-output-to-string (s)
                 (mapcar (lambda (l) (format s "~A" l)) result)))))

(defun count-first-char (str &optional (start 0))
  "Return a count of how many times the first character in a string
occurs, without interuption. Returns 0 for an empty string."
  (do* ((count 0 (1+ count)))
       ((or (>= count (- (length str) start))
            (not (equal (char str (+ start count))
                        (char str start))))
       count)))
