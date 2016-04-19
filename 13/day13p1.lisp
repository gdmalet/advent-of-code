;;;; Advent of code, day 13 problem 1.
;;;; Answer is 618.

;;;; This is an adaptation of problem 9 (the Travelling Salesman), but
;;;; with possibly different distances between pairs of towns,
;;;; depending on direction. Also, we must return to the start point.
;;;; -----------------------------------------------------------------

;;;; This solution is nearly identical to d09p2, except "shortest" is
;;;; changed to "longest", and < changed to >.

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

(defparameter *greatest-happiness* (cons -1 nil)
  "The longest distance we have so far.")

(defun main ()
  "Do all initialisation, then find a solution."

  ;; Load input into a hash table for easier processing
  (fetch-input *input-file*)

  ;; Unlike day09p[12], we don't need to try every possible starting
  ;; point, since here everyone is sitting in a circle, so it doesn't
  ;; matter where we start.
  (loop
     for key being the hash-keys in *input*
     do
       (start-at (gethash key *input*)  ; happiness delta for each person
                 (list key)             ; arrangement so far
                 0                      ; happiness so far
                 (1- (hash-table-count *input*))) ; how deep we must go
       (return))  ; ----->> bail after the first time

  (format t "Greatest happiness: ~D -> ~A~%"
          (car *greatest-happiness*)
          (cdr *greatest-happiness*)))

(defun start-at (people arrangement happiness depth)

  (when (= depth 0)                  ; bottomed out, so close the loop
    (incf happiness
          (happiness-delta (car arrangement)
                           (car (last arrangement))))
    ;;(format t "arrangement: ~D: ~A~%" happiness (reverse arrangement))
    (when (> happiness (car *greatest-happiness*))
      (format t "best: ~D: ~A~%"  happiness (reverse arrangement))
      (setf *greatest-happiness* (cons happiness (nreverse arrangement))))
    (return-from start-at))

  ;; Find a person we haven't seated yet, add them to the arrangement,
  ;; with the happiness delta.
  (loop
     for local-arrangement = (copy-list arrangement)
     for person in people
     do
       (unless (find (car person) arrangement :test #'equal)
         (start-at (gethash (car person) *input*)
                   (push (car person) local-arrangement)
                   (+ happiness
                      (cdr person)
                      (happiness-delta-l2r (car person) (car arrangement)))
                   (1- depth)))))

(defun happiness-delta-l2r (a b)
  "Return happiness delta for person a when they sit next to
b (ignoring vice versa)."
  (cdr (find-if
        (lambda (pair)
          (equal (car pair) b))
        (gethash a *input*))))

(defun happiness-delta (a b)
  "Return the happiness delta when a sits next to b, by looking at the
delta in both directions."
    (+ (happiness-delta-l2r a b)
       (happiness-delta-l2r b a)))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns a hash table with each
key being a person, and the values a list of dotted pairs of person
and happiness delta. Updates *input*."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (fetch-line f)
       while line
       do
         (push (cons (nth 1 line) (nth 2 line))
               (gethash (nth 0 line) *input* '()))
       finally
         (return *input*))))

;;; This gaily assumes all input is in the format:
;;; Alice would {gain|lose} 57 happiness units by sitting next to Bob.
(defun fetch-line (f)
  "Fetch a line of input from stream f. Returns triplet of
two names and the happiness delta."
  (loop
     repeat 11
     for word = (read f nil)
     unless word return nil
     collect word into words
     finally
       (return
               (list
                (car words)
                (drop-symbol-dot (nth 10 words))
                (if (equal (nth 2 words) 'GAIN) ; else 'LOSE
                    (nth 3 words)
                    (- (nth 3 words)))))))

(defun drop-symbol-dot (sym)
  "Drop trailing dot from a symbol name."
  (let ((string (symbol-name sym)))
    (with-input-from-string (s
                             (substring string
                                        0
                                        (1- (length string))))
    (read s))))
                                                    
