;;;; Advent of code, day 20 problem 1.
;;;; Answer is 665280.

;;; Sample output:
;; [...]
;; new candidate: total 29145720 at 1275120
;; new candidate: total 29545200 at 1164240
;; new candidate: total 30625200 at 1053360
;; new candidate: total 29393280 at 887040
;; new candidate: total 29260800 at 665280
;; First door with > 29000000 presents is 665280, with 29260800
;; Real time: 72.198105 sec.
;; Run time: 72.19512 sec.
;; Space: 116233720 Bytes
;; GC: 2, GC time: 0.009103 sec.

(defconstant *puzzle-input* 29000000    ; 29 million
  "Puzzle input from the problem statement.")

;; Simple walk each elf along the street, depositing presents, until
;; we go over the limit. Make sure all elves go to that point, but no
;; further.
(defun brute-sieve ()
  (loop
     ;; This limit should be something big. Looking at output.txt, the
     ;; average ratio of the puzzle input to the winning door number
     ;; is a bit over 40 -- so be a bit convervative.
     ;; Note that this value is too small for puzzle inputs under about 1000.
     with over-limit = (ceiling (/ *puzzle-input* 30))
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
            while (< i-inner over-limit)
            do
              (incf (aref sums i-inner) (* i 10))
              (when (> (aref sums i-inner) *puzzle-input*)
                (format t "new candidate: total ~D at ~D~%"
                        (aref sums i-inner) i-inner)
                (setf over-limit i-inner) ; reduce the search area....
                (return-from inner-loop))))
       finally (format t "First door with > ~D presents is ~D, with ~D~%"
          *puzzle-input* over-limit (aref sums over-limit))))


;;; Following is all kinds of stuff which turns out to be completely
;;; unnecessary. A simple Sieve of Eratostenes-style search does the
;;; trick.

(defvar *primes* nil
  "A bunch of generated prime numbers, for finding prime factors.")

(defparameter *results* nil)

;;; A brute force search like this takes too long. See output.txt,
;;; generated from calling this function.
(defun main ()
  (loop
     with max = 0 and maxdoor
     for door from 1
     for num = (presents-for-door door)
     until (= num
              *puzzle-input*)
     do
       (when (> num max)
         (format t "door ~D is max ~D (~5,2F)~%" door num (/ num door))
         (setf max num maxdoor door))
     finally
       (format t "First door: ~D~%" door)))

(defun presents-for-door (door)
  "Return how many presents are delivered to this door."
  (flet ((times-ten (x)
           (* x 10)))
    (apply '+
           (mapcar #'times-ten
                   (divisors door)))))

;; 90000003 breaks
(defun test (lim &optional (start 1))
  (loop for door from start to lim
     do
       (format t "~D " door)
       (unless (equal
                (slowly-which-elves-visit door)
                (which-elves-visit door))
         (error "broke at ~D" door))))

(defun which-elves-visit (door)
  ;;(format t "which elves ~D -> ~A~%" door
  ;;        (sort (divisors door) #'<))
  (sort (divisors door) #'<))

(defun slowly-which-elves-visit (door)
  ;;(format t "which sloes ~D -> ~A~%" door
  ;;        (sort (slow-divisors door) #'<))
  (sort (slow-divisors door) #'<))

(defun combo (list)
  (when (= (length list) 1)
    (push (car list) *results*)
    ;;(format t "pushb ~A~%" *results*)
    (return-from combo *results*))

  (combo (cdr list))
  ;;(format t " -> *results* ~A~%" *results*)

  (loop
     for num in (copy-list *results*)
     do
       (push (* num (car list)) *results*))
       ;;(format t "push ~A~%" *results*))

  (push (car list) *results*)

  *results*)

(defun divisors (num)
  (when (= num 1)
    (return-from divisors '(1)))
  (let ((f (factors num)))
    (when f
      (setf *results* '())
      (combo (factors num))
      (push 1 *results*)
      (return-from divisors
        (delete-duplicates *results*)))
    (list 1 num)))

(defun slow-divisors (num)
  (when (= num 1)
    (return-from slow-divisors '(1)))

  (loop for x from 2 to (ceiling (/ num 2))
     when (= (mod num x) 0)
       collect x into divs
     finally
       (return (concatenate 'list (list 1 num) divs))))

(defun factors (num)
  "Return all the factors of num."
  (loop
     with f = '()
     for p in (prime-factors num)
     do
       (loop
          while (= (mod num p) 0)         ; it divides
          do
            (setf num (/ num p))
            (push p f))
     finally
       (return (nreverse f))))

(defun prime-factors (num)
  "Return the prime factors of num."
  ;; We need a bunch of primes, so generate a bunch first time though.
  (unless *primes*
    (format t "................ Generating primes~%")
    (setf *primes*
          (prime-sieve (ceiling (sqrt *puzzle-input*)))))
  ;;(assert (< num (* 2 (car (last *primes*)))))

  (loop
     for p in *primes*
     while (<= p (/ num 2))
     when (= (mod num p) 0)
     collect p))

;;; Sieve of Eratosthenes
;;; See https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(defun prime-sieve (max)
  "Generate all primes less than or equal to max."
    (loop
       with ary = (make-array (1+ max)
                              :element-type 'bit
                              :initial-element 1)
       for i from 3 by 2 to max ; skipping evens
       when (= (aref ary i) 1)
       do
         (loop
            for j = (* i i) then (+ j i)
            while (<= j max)
            do
              (setf (aref ary j) 0))
       finally
         (return (append '(2)
                        (loop
                           for i from 3 to max by 2
                           when (= (aref ary i) 1)
                           collect i)))))
