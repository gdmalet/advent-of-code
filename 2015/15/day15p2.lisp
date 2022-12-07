;;;; Advent of code, day 15 problem 2.
;;;; Answer is 15862900. 

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defconstant *total-teaspoons* 100
  "How many teaspoons there are in a given recipe.")

(defconstant *target-calories* 500
  "How many calories we want in the best recipe.")

(defvar *ingredients* nil
  "Ingredients gleaned from the above file.")

(defvar *properties* nil
  "All the ingredient properties that we're interested in.")

(defvar *best* nil "Our best guess so far.")

(defun main (&optional (total-teaspoons *total-teaspoons*))
  (setf *ingredients* (fetch-input *input-file*))

  ;; Assume the first ingredient has all the properies we care about,
  ;; but ignore calories for now.  This list is not really necessary
  ;; since the positions of the properties in the input file are
  ;; constant, so we could use that, but at least this makes it more
  ;; flexible.
  (setf *properties*
        (delete 'calories
                (mapcar #'car (cdar *ingredients*))))

  ;; Find the best combination
  (setf *best* (cons 0 nil))
  (generate (make-array (length *ingredients*)) 
            total-teaspoons
            (length *ingredients*))

  (format t "Best score is ~D for ~D teaspoons, ~D calories in ~A~%"
          (car *best*) total-teaspoons *target-calories* (cdr *best*)))

(defun check-recipe (ary)
  "See if the given recipe is our best yet. Updates *best*."
  (let ((score (recipe-total ary)))
    (when (> score (car *best*))
      (let ((calories
             (loop
                for i from 0 to (1- (array-total-size ary))
                sum (ingredient-teaspoon-score (nth i *ingredients*)
                                               (aref ary i)
                                               'CALORIES)
                into property-total
                finally
                  (return property-total))))

      (when (= calories *target-calories*)
        (format t "best: ~D (~D) ~A~%" score calories ary)
        (setf *best*
              (cons score (coerce ary 'list))))))))
    
(defun recipe-total (ary)
  "Return the total score over all the ingredients for a given recipe."
  (loop
     with product = 1
     for property in *properties*   ; step over each property
     collect
       (loop                         ; sum the property over all ingredients
          for i from 0 to (1- (array-total-size ary))
          sum (ingredient-teaspoon-score (nth i *ingredients*)
                                             (aref ary i)
                                             property)
            into property-total
          finally
            (return property-total))
     into property-totals
     finally
       (mapc (lambda (x)
               (if (< x 0)            ; negative scores become 0
                   (setf product 0)
                   (setf product (* product x))))
             property-totals)
       (return product)))

;; There are 176851 was to generate 4 numbers that add up to 100,
;; where order matters.... which takes a fraction of a second.
(defun generate (ary sum slots)
  "Generate all possible ways of summing up to the given sum, in given
number of slots, using the array to store values."
  (case slots
    ;; With 1 slot, there's only 1 solution
    (1
     (setf (aref ary 0) sum)
     (check-recipe ary))
    ;; With more slots, generate all values for the first slot, then
    ;; for each generate a solution for n-1 slots for the (sum - value).
    (t
     (loop
        for value from 0 to sum
        do
          (setf (aref ary (1- slots)) value)
          (generate ary (- sum value) (1- slots))))))

(defun ingredient-teaspoon-score (ingredient num-spoons property)
  "For the given ingredient property list, return the score for
num-spoons of the given property."
  (* num-spoons
     (cdr
      (find-if
       (lambda (p)
         (equal (car p)
                property))
       (cdr ingredient)))))

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
       collect (parse-line line) into lines
       finally
         (return lines))))

;;; This gaily assumes all input is in the format:
;;; Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 3
(defun parse-line (line)
  "Parse a line of input. Each line is returned a list of an
ingredient name, then pairs of (property . value)s."

  (with-input-from-string (s line)

    ;; The lisp reader would do silly things on a raw comma so make it
    ;; a delimiter which is otherwise ignored. Ignore colons too.
    (let ((*readtable* (copy-readtable)))
      (flet ((ignore-char (stream char)
               (declare (ignore char))
               (read stream nil nil t)))
        (set-macro-character #\, #'ignore-char)
        (set-macro-character #\: #'ignore-char)

        (loop
           with ingredient = (read s nil)
           for property = (read s nil)
           for value = (read s nil)
           unless property
           return
             (cons ingredient properties)
           collect
             (cons property value) into properties)))))
