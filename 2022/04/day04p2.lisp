;;;; Advent of code, day 4 problem 2.
;;;; Answer is 938.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun split-range (range)
  "Splits string of two dash-separated numbers, and return the numbers."
  
  ;; Make our own copy of the readtable, so we can change it
  (let ((*readtable* (copy-readtable)))
	;; turn the dash into a separator
	(set-macro-character
	 #\- (lambda (stream char)
		   (declare (ignore char))
		   (read stream t nil t)))

	(multiple-value-bind (left end)
		(read-from-string range)
	  (values left (read-from-string range nil nil :start end)))))

(defun process-line (left right)
  "Returns 1 if there is overlap in ranges, else 0."
  (multiple-value-bind (left-from left-to)
	  (split-range left)
	(multiple-value-bind (right-from right-to)
		(split-range right)
	  
	  (if (or (and (>= left-from right-from)
				   (<= left-from right-to))
			  (and (>= left-to right-from)
				   (<= left-to right-to))
			  (and (>= right-from left-from)
				   (<= right-from left-to))
			  (and (>= right-to left-from)
				   (<= right-to left-to)))
		  1
		  0))))

(defun main ()
   (with-open-file
       (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

	 ;; Make our own copy of the readtable, so we can change it
	 (let ((*readtable* (copy-readtable)))

	   ;; The lisp reader would normally barf on a raw comma, so instead
	   ;; make it a delimiter which is otherwise ignored.
	   (set-macro-character
		#\, (lambda (stream char)
			  (declare (ignore char))
			  (read stream t nil t)))
 
     (loop
       for left = (read f nil)
       for right = (read f nil)
       while left
       summing (process-line (symbol-name left)
							 (symbol-name right)) into total
       finally (return total)))))
