;;;; Advent of code, day 7 problem 1.
;;;; Answer is 46065.

;;;; Rather than trying to sort the input into optimum order, or
;;;; building a tree of dependencies, instead just brute force through
;;;; the input file, and keep reprocessing the list until the circuit
;;;; stablilizes -- which in this case it does after something like 33
;;;; iterations.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *wires* (make-hash-table :size 1024 :test 'EQUAL)
  "Used to track the signals on each wire.")

(defun main ()
  "Repeat processing the circuit until it stabilizes (i.e. no changes)."
  (loop
     for count = (process-input-strings *input-file*)
     do
       (format t "changes: ~A~%" count)
     while (> count 0))

  (format t "The signal provided to wire a is ~A.~%"
          (gethash 'a *wires*)))

(defun process-input-strings (from-file)
  "Process each line from the input file, one at a time. Returns how
many changes were made to the circuit state."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)
    (loop
       for line = (read-line f nil)
       while line
       count (process-line line) into count
       finally (return count))))

(defun process-line (line)
  "Process one line of input through a pretty crude parser."
  (loop
     with start = 0 and shift-dir = 0
     with defun = nil and inputs = '()
     do
       (multiple-value-bind (token next)
           (get-token line start)

         (case token
           (OR
            (setf defun 'logior))
           (AND
            (setf defun 'logand))
           (NOT
            (setf defun 'lognot-u16))   ; must not do sign-extension.
           (LSHIFT
            (setf defun 'ash shift-dir 1))
           (RSHIFT
            (setf defun 'ash shift-dir -1))

           (->
            (let* ((rhs (get-token line next))
                   (old-value (gethash rhs *wires* 0))
                   (result
                    (if defun           ; need to do a funcall
                        (eval `(,defun ,@(nreverse inputs)))
                        (car inputs)))) ; just a simple assignment a -> b

              ;; Return T if there was a change, else nil
              (return (not
                       (eql old-value
                            (setf (gethash rhs *wires*) result))))))


           (t           ; a LHS value or symbol that is not an operation
            (unless (eql shift-dir 0)           ; have a left or a right shift
              (setf token (* token shift-dir))) ; negative to shift right

            ;; Save the rhs result into the hash table.
            (push (if (integerp token)
                      token                      ; it's already an int
                      (gethash token *wires* 0)) ; fetch int value
                  inputs))) ; creates a reversed list of inputs.

         (setf start next))))

(defun get-token (line start)
  "Return the next token from the input line, and the next start position.
The token is nil when there's nothing left to read."
  ;; Don't need to do anything clever. Just use the lisp reader.
  (read-from-string line nil nil :start start))

(defun lognot-u16 (int)
  "Return a 16-bit unsigned logical not (with no sign extension etc.).
The input is first truncated to 64 bits, then all are flipped."
  (let ((mask (1- (expt 2 16))))
    (logxor (logand int mask) mask)))
