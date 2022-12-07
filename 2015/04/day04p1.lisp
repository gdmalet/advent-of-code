;;;; Advent of code, day 4 problem 1.
;;;; Answer is 346386.
;;;;
;;;; Calling an exernal program is not portable in common lisp.
;;;; We use GNU's CLISP run-program option here.
;;;; This does an awful lot of forking (one per attempt), so is not as
;;;; fast as one could hope, but my laptop finds the answer in under 5
;;;; minutes.

(defconstant *puzzle-input* "iwrupvqb"
  "Hash input prefix.")

(defun main ()
  (format t "First acceptable Advent Coint suffix is ~A.~%"
          (mine-advent-coins *puzzle-input*)))

(defun mine-advent-coins (secret-key)
  "Return the first number which as a suffix to the secret key returns
an acceptable hash."

  (loop
     for n from 1                       ; until infinity...
     when
       (acceptable-hash
        (get-md5-hash
         (format nil "~A~A" secret-key n)))
       return n))

(defun acceptable-hash (hash)
"Returns true if the hash has at least 5 leading zeros, else nil."
  (equal (substring hash 0 5) "00000"))

(defun get-md5-hash (input)
  "Returns an md5 hash of the given input string by calling the OS's
md5sum program. Uses the non-portable CLISP ext:run-program.
Example input pqrstuv104897 should produce output
000006136ef2ff3b291c85725f17325c"

  (multiple-value-bind (io i o)
      (ext:run-program "md5sum" :OUTPUT :STREAM :INPUT :STREAM)
    (format o input)                    ; Send the string to md5sum
    (close o)                           ; Must do this here so md5
                                        ; will write its output
    (let ((output (read-line i)))       ; Get the md5 hash, and close
      (close i)                         ; the other descriptors.
      (close io)

      ;; Trim the return value to 128 hex digits,
      ;; so drop the trailing filename.
      (substring output 0 32))))
