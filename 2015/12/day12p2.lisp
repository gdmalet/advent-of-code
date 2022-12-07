;;;; Advent of code, day 12 problem 2.
;;;; Answer is 96852.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defun main ()
  (format t "Sum of all numbers in all non-red objects in the document is ~D.~%"
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

;; Use a stack to track the total of the current object, and if we're
;; working on an array or object. If we hit a red object, discard any
;; current total, and that of all the child objects. Track if any
;; parent objects are red by storing the letter 'r' on the stack if
;; so, and counting how many.
(defun process-line (line)
  "Process one line of input."
  (loop
     with start = 0 and object-total = 0 and red-count = 0
     with stack = '()
     do
       (multiple-value-bind (token next)
           (get-token line start)

         (unless token
           (return object-total))

         ;; (format t "token: ~7A r~D   ~D  ~A~%"
         ;;      token red-count object-total stack)

         (if (and (integerp token)      ; accumulate total for non-red ojects
                  (= red-count 0))
             (incf object-total token))

         (when (and (equal token "red")
                    (eql (car stack) #\{))        ; parsing a non-red object
           (incf red-count)                       ; else stack would contain r
           (setf object-total 0)                  ; discard total
           (push #\r stack))

         ;; This is good enough to work, but will happily consume a
         ;; badly formatted document, and trip up on something like "red":42.
         (if (equal (type-of token) 'STANDARD-CHAR)
             (ecase token
               (#\[                     ; start array
                (push token stack))

               (#\]                     ; end array
                (assert (eql (pop stack) #\[)))

               (#\{                     ; start object
                (push object-total stack) ; save total and a {
                (push token stack)
                (setf object-total 0))  ; start new child object

               (#\}                     ; end object
                (when (eql (car stack) #\r)
                  (decf red-count)      ; accumulate child total with ours
                  (assert (= object-total 0))
                  (pop stack))
                (assert (eql (pop stack) #\{))
                (incf object-total (pop stack)))))

         (setf start next))))

(defun get-token (line start)
  "Return the next token from the input line, and the next start position.
The token is nil when there's nothing left to read."

  ;; Make our own copy of the readtable, so we can change it
  ;; Should probably do this once at the start, rather than for every read.
  (let ((*readtable* (copy-readtable)))

    ;; The lisp reader would do silly things on some chars,
    ;; so make them a delimiter and return some, ignore others.
    (flet ((ignore-char (stream char)
             (declare (ignore char))
             (read stream nil nil t))

           (return-char (stream char)
             (declare (ignore stream))
             char))

      (set-macro-character #\, #'ignore-char)
      (set-macro-character #\: #'ignore-char)
      (set-macro-character #\[ #'return-char)
      (set-macro-character #\] #'return-char)
      (set-macro-character #\{ #'return-char)
      (set-macro-character #\} #'return-char)

      (read-from-string line nil nil :start start))))
