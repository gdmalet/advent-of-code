;;;; Advent of code, day 20 problem 2.
;;;; Answer is 705600.

;; new candidate: total 29050053 at 3255840
;; new candidate: total 30314778 at 2162160
;; new candidate: total 29374663 at 1801800
;; new candidate: total 30358107 at 1663200
;; new candidate: total 30179005 at 1441440
;; new candidate: total 29173924 at 1310400
;; new candidate: total 29341168 at 1108800
;; new candidate: total 30092380 at 942480
;; new candidate: total 29002456 at 705600
;; First door with > 29000000 presents is 705600, with 29002456
;; Evaluation took:
;;   0.177 seconds of real time
;;   0.177016 seconds of total run time (0.177016 user, 0.000000 system)
;;   [ Run times consist of 0.005 seconds GC time, and 0.173 seconds non-GC time. ]
;;   100.00% CPU
;;   459,609,835 processor cycles
;;   116,000,016 bytes consed

(defconstant *puzzle-input* 29000000    ; 29 million
  "Puzzle input from the problem statement.")
(defconstant *max-houses* 50
  "Only visit this many houses.")

;; Simply walk each elf along the street, depositing presents, until
;; we go over the limit. Make sure all elves go to that point, but no
;; further.
(defun brute-sieve ()
  (loop
     with over-limit = *puzzle-input*
     with sums = (make-array over-limit
                             :element-type '(integer 0 4294967295)
                             :initial-element 10)
     for i from 2
     while (< i over-limit)
     do
       ;;(format t "Index ~D, limit ~D~%" i over-limit)
       (block inner-loop
         (loop
            for i-inner = i then (+ i-inner i)
            while (and (<= i-inner (* i *max-houses*))
                       (<  i-inner over-limit))
            do
              (incf (aref sums i-inner) (* i 11))
              (when (> (aref sums i-inner) *puzzle-input*)
                (format t "new candidate: total ~D at ~D~%"
                        (aref sums i-inner) i-inner)
                (setf over-limit i-inner) ; reduce the search area....
                (return-from inner-loop))))
       finally (format t "First door with > ~D presents is ~D, with ~D~%"
          *puzzle-input* over-limit (aref sums over-limit))))

