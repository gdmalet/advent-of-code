;;;; Advent of code, day 17 problem 1.
;;;; Answer is 4372.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *total-liters* 150
  "How much liquid we start with.")

(defparameter *solution-count* 0
  "How many solutions we found.")

(defun main (&optional (total *total-liters*))
  "Entry point. Find the ways of filling our containers."
  (fill-containers total
                (fetch-input *input-file*)
                nil)

  (format t "There are ~D ways of filling the containers with ~d liters.~%"
          *solution-count*
          total))

;; Assumes the list of containers is in descending size order.
(defun fill-containers (amount containers filled)
  "Find all the ways of filling the containers with the given amount
of liquid. Increments *solution-count* for each solution."

  ;; Bottomed out: used more liquid than actually exists
  (if (< amount 0)
    (return-from fill-containers nil))

  ;; Jolly good: used exactly the correct amount of liquid
  (when (= amount 0)
    (format t " -- solution ~A~%" (reverse filled))
    (incf *solution-count*)
    (return-from fill-containers t))
  
  ;; Else we have some liquid and hopefully containers left, so fill
  ;; each possible container, and recurse to see if the remaining
  ;; liquid can exactly fill the remaining containers.
  (loop
     for container = (pop containers)
     while container
     do
       (when (<= container amount)
         (fill-containers (- amount container)
                       containers
                       (push container filled))
         (pop filled))
     finally
       (return nil)))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns a list of container
sizes, in descending size order."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for size = (read f nil)
       while size
       collect size into sizes
       finally (return (sort sizes #'>)))))

