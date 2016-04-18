;;;; Advent of code, day 12 problem 1.
;;;; Answer is 156366.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defun main ()
  (format t "Sum of all numbers in the document is ~D~%"
          (process-input-strings *input-file*)))

;; This is a bit of overkill for this problem, since there's only one
;; line of input.
(defun process-input-strings (from-file)
  "Process each line from the input file, one at a time."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)
    (loop
       for line = (read-line f nil)
       while line
       sum
         (process-line line) into total
       finally
         (return total))))

(defun process-line (line)
  "Process one line of input. Sum anything that's an integer, and
ignore the rest."
  (loop
     with start = 0 and total = 0
     do
       (multiple-value-bind (token next)
           (get-token line start)

         (if (integerp token)
             (incf total token))

         ;; (format t "token: ~A -- ~A ~A (~D)~%" token (type-of token)
         ;;         (if (integerp token)
         ;;             " --> integer"
         ;;             "")
         ;;         total)

         (unless token
           (return total))

         (setf start next))))


(defun get-token (line start)
  "Return the next token from the input line, and the next start position.
The token is nil when there's nothing left to read."

  ;; Make our own copy of the readtable, so we can change it
  ;; Should probably do this once at the start, rather than for every read.
  (let ((*readtable* (copy-readtable)))

    ;; The lisp reader would do silly things on some chars,
    ;; so make them a delimiter which is otherwise ignored.
    (flet ((ignore-char (stream char)
             (declare (ignore char))
             (read stream nil nil nil)))

      (set-macro-character #\, #'ignore-char)
      (set-macro-character #\: #'ignore-char)
      (set-macro-character #\[ #'ignore-char)
      (set-macro-character #\] #'ignore-char)
      (set-macro-character #\{ #'ignore-char)
      (set-macro-character #\} #'ignore-char)

      (read-from-string line nil nil :start start))))
