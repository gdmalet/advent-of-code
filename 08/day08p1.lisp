;;;; Advent of code, day 8 problem 1.
;;;; Answer is 1333.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defun main ()
  "Process all the strings, and do the math."
  (format t "Literal string length minus actual length is ~D.~%"
          (process-input-strings *input-file*)))

(defun process-input-strings (from-file)
  "Process each line from the input file, one at a time."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (read-line f nil)
       while line
       sum                      ; the 2 below is the enclosing quotes.
         (- (length line)
            (length (format-string line)) -2)
       into diff
       finally
         (return diff))))

(defun format-string (line)
  "Return the properly escaped input string."
  ;; Step through the existing string, and build up a new one to return."
  (loop
     for i from 0 to (1- (length line))
     collect
       (case (char line i)
         (#\\                           ; it's a backslash
          (incf i)                      ; look at the next char
          (ecase (char line i)
            (#\\ #\\)                   ; \\ returns one backslash
            (#\" #\")                   ; \" returns the quote
            (#\x                        ; \x27 returns char 0x27
             (incf i)
             (let ((retval
                    (coerce
                     (parse-integer
                      (substring line i (+ i 2)) :radix 16)
                     'character)))
               (incf i)                ; will skip past the two hex digits
               retval))))              ; return the character we saved
         (t                            ; Anything other than backslash
          (char line i)))              ;  is return as-is.
       into chars
     finally
       (return
         (coerce chars 'string))))
