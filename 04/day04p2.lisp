;;;; Advent of code, day 4 problem 2.
;;;; Answer is 9958218. Took 2h15 mins on my laptop.
;;;;
;;;; This is really problem 1, modified slightly so it can handle both
;;;; problem one and two. Also some small tweaks to try get some speed
;;;; increase, but not to much avail.

(defconstant *puzzle-input* "iwrupvqb"
  "Hash input prefix.")

(defun main ()
  (format t "First acceptable 5 zeroes Advent Coin suffix is ~A.~%"
          (mine-advent-coins *puzzle-input* 5))

  ;; Should really start where the prior one left off....
  (format t "First acceptable 6 zeroes Advent Coin suffix is ~A.~%"
        (mine-advent-coins *puzzle-input* 6)))

(defun mine-advent-coins (secret-key num-zeroes)
  "Return the first number which as a suffix to the secret key returns
an acceptable hash."

  (loop
     for n from 1                       ; until infinity...
     do
       (multiple-value-bind (q r)
           (floor n 10240)
         (declare (ignore q))
         (when (eql r 0)
           (format t "prefix ~A~%" n)))
     when
       (acceptable-hash
        (get-md5-hash
         (format nil "~A~A" secret-key n))
        num-zeroes)
       return n))

(defun acceptable-hash (hash num-zeroes)
"Returns true if the hash has at least num-zeroes leading zeros, else nil."

  (equal (substring hash 0 num-zeroes)
         (make-string num-zeroes :initial-element #\0)))

(defun get-md5-hash (input)
  "Returns an md5 hash of the given input string by calling the OS's
md5sum program. Uses the non-portable CLISP ext:run-program.
Example input pqrstuv104897 should produce output
000006136ef2ff3b291c85725f17325c"

  (multiple-value-bind (io i o)
      (ext:run-program "/usr/bin/md5sum" :OUTPUT :STREAM :INPUT :STREAM)
    (princ input o)                     ; Send the string to md5sum
    (close o)                           ; Must do this here so md5
                                        ; will write its output
    (let ((output (read i)))            ; Get the md5 hash, and close
      (close i)                         ; the other descriptors.
      (close io)

      output)))

      ;; Alternatively, for the purists, trim the return value to 128
      ;; hex digits, so drop the trailing filename.
      ;(substring output 0 32))))
