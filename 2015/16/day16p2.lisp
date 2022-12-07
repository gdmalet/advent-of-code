;;;; Advent of code, day 16 problem 2.
;;;; Answer is 241.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *items* (make-hash-table :test 'equal)
  "Read from above file. Key is an item, value is a pair (Sue#
  . count).")

(defconstant *num-aunts* 500 "How many Aunt Sues there are.")

(defconstant *ticker-tape*
  '((children 3 =)
    (cats  7 >)
    (samoyeds 2 =)
    (pomeranians 3 <)
    (akitas 0 =)
    (vizslas 0 =)
    (goldfish 5 <)
    (trees 3 >)
    (cars 2 =)
    (perfumes 1 =))
  "The contents of the ticker tape, listing our search criteria, as
  well as how we compare values.")

(defun main ()
  "Start point. Load the input and do a search."
  (fetch-input *input-file*)

  (let ((a (make-array (1+ *num-aunts*)  ; will index from base 1
                       :initial-element 0
                       :element-type 'bit)))

    ;; Identify all the excluded aunts by setting a bit in the array.
    (loop
       with excluded = (excluded-aunts)
       for aunt in excluded
       do
         (setf (aref a aunt) 1))

    (format t "Aunt numbers matching search criteria:")
    ;; Print those without the corresponding bit set
    (loop for i from 1 to (1- (length a))    ; didn't use index 0
       do
         (if (= (aref a i) 0)
             (format t " ~D" i)))
    (format t "~%")))

(defun excluded-aunts ()
  "Weed out all the aunts that don't have a required item
count. Returns a list of them."
  (loop
     for union = '()
        then (union union (find-aunts-without item-count-pair))
     for item-count-pair in *ticker-tape*
     finally
       (return union)))

(defun find-aunts-without (item-count-pair)
  "Return a list of all the aunts that don't have the required count
of the item, as determined by the passed in function. This considers
only those aunts that have some of the item recorded."
  (loop
     with item = (car item-count-pair)
        and count = (cadr item-count-pair)
        and compare = (caddr item-count-pair)
     for pair in (gethash item *items*)
     unless (funcall compare (cdr pair) count)
       collect (car pair) into aunts
     finally
       (return aunts)))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns a list of lists of
properties for each ingredient."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (read-line f nil)
       while line
       do
         (parse-line line))))

;;; This gaily assumes all input is in the format:
;;; Sue 1: goldfish: 9, cars: 0, samoyeds: 9
(defun parse-line (line)
  "Parse a line of input. Updates *items* with
(aunt# . count)s for each item key."

  (with-input-from-string (s line)

    ;; The lisp reader would do silly things on a raw comma so make it
    ;; a delimiter which is otherwise ignored. Ignore colons too.
    (let ((*readtable* (copy-readtable)))
      (flet ((ignore-char (stream char)
               (declare (ignore char))
               (read stream nil nil t)))
        (set-macro-character #\, #'ignore-char)
        (set-macro-character #\: #'ignore-char)

        (read s)                        ; discard "Sue"

        (loop
           with sue-number = (read s nil)
           for item = (read s nil)
           for count = (read s nil)
           unless item return nil
           do
             (push (cons sue-number count)
                   (gethash item *items* '())))))))
