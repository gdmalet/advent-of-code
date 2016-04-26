;;;; Advent of code, day 19 problem 1.
;;;; Answer is 576.

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *results-table* (make-hash-table
                              :test 'equal
                              :size 64)
  "Store which unique molecules have been found.")

(defun main ()
  "Entry point."
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)

    ;; Step through the medicine, replacing each atom in turn.
    (loop
       with start = 0
       while (< start (length medicine))
       do
         (multiple-value-bind (atom next)
             (get-next-atom medicine start)
           (replace-atom medicine start next atom replacements)
           (setf start next))))
  
  (format t "~D final results~%" (hash-table-count *results-table*)))

(defun replace-atom (medicine start end atom replacements)
  "Replace an atom one at a time with its possible
replacements. Updates *results-table* with unque results."
  (loop
     with repls = (find atom replacements
                        :test #'(lambda (a r) (equal (car r) a)))
     for rep in (cdr repls)
     for result = (concatenate 'string
                               (substring medicine 0 start)
                               rep
                               (substring medicine end))
     do
       (setf (gethash result *results-table*) t)))

;; Each atom is either an uppercase letter, or only two letters, the
;; first uppercase, the second lowercase.
(defun get-next-atom (string start)
  "Returns the next atom from the passed in molecule, and the next
start position."
  (if (>= start (length string))
      (return-from get-next-atom nil))

  (let* ((c (schar string start))       ; the character
        (n (1+ start))                  ; start of the next character
        (x (if (< n (length string))    ; the next character
               (schar string n)
               nil)))
    (if (and x (lower-case-p x))
        (values (format nil "~C~C" c x)
                (+ start 2))
        (values (format nil "~C" c)
                (+ start 1)))))


;;;; This assumes input is of the form
;;;; Al => ThF
;;;; with a trailing blank line, then one long chemical list.
(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns the medicine molecule,
and a list of lists, each list being an item followed by its possible
replacements."

  ;; Need to mess with the readtable, so take a copy.
  (let ((*readtable* (copy-readtable))
        (all-items '()))
    (setf (readtable-case *readtable*) :preserve)

    (with-open-file
        (f (make-pathname :name from-file)
           :direction :input
           :if-does-not-exist :error)

      (loop
         with items = nil
         for previous = nil then from
         for from = (symbol-name (read f))
         for delim = (read f nil)       ; =>
         for to = (symbol-name (read f nil))
         while delim                    ; is nil at the blank line
         do
           (when (not (equal from previous))
             (if items
                 (push (cons previous items) all-items))
             (setf items '()))
           (push to items)
         finally
           (push (cons previous items) all-items)
           (return (values from all-items))))))
