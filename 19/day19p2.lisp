;;;; Advent of code, day 19 problem 2.
;;;; Answer is ?

(defconstant *input-file* "input4.txt"
  "Where we read the strings.")

(defparameter *atom-replacements* '())
(defparameter *RnAr-replacements* '())
(defparameter *count* 0)
(defparameter *ptr* 0)

(defun main ()
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)
    (format t "med (~D): ~A~%~%repl (~D): ~A~%~%"
            (length medicine) medicine (length replacements) replacements)

    (setf *atom-replacements* nil *RnAr-replacements* nil)
    (separate-replacements (reverse-replacements replacements))

    ;;(setf *atom-replacements* (nreverse *atom-replacements*))

    (format t "atom-repl (~D): ~A~%~%RnAr-repl (~D): ~A~%~%"
            (length *atom-replacements*) *atom-replacements* 
            (length *RnAr-replacements*) *RnAr-replacements*)

    (setf *count* 0)
    (setf *ptr* 0)

    (rdp medicine "" '())))
    ;;(generate medicine '())))

(defun rdp (medicine this-molecule changes)
  "Recursive descent parser"
  (incf *count*)
  (loop
     with molecule = this-molecule and last-atom
     while *ptr*
     do
       (multiple-value-bind (atom next)
           (get-next-atom medicine *ptr*)
         (format t "~D: ptr ~D, next ~D, atom ~A, last-atom ~A, mol ~A~%"
                 *count* *ptr* next atom last-atom molecule)

         (block loop-inner
           (when (equal atom "Rn")
             (format t "~D: atom Rn, molecule ~A~%" *count* molecule)
             (setf *ptr* next)
             (setf molecule (do-simple-replacements
                                (concatenate 'string
                                             molecule
                                             last-atom)))
             (format t "~D: Rn2 mol is now ~S~%" *count* molecule)
             (let ((last-atom-offset
                    (- (length molecule)
                        (if (lower-case-p
                             (char molecule (1- (length molecule))))
                            2
                            1))))
               (setf molecule (concatenate 'string
                                           (substring molecule 0 last-atom-offset)
                                           (rdp medicine
                                                (concatenate 'string (substring molecule last-atom-offset) atom)
                                                changes))))
             (format t "~D: Rn3  mol is now ~S~%" *count* molecule)
             ;;(setf molecule (do-simple-replacements molecule))
             ;;(format t "~D: Rn2 mol is now ~S~%" *count* molecule)
             (let ((last-atom-offset
                    (- (length molecule)
                       (if (lower-case-p
                            (char molecule (1- (length molecule))))
                            2
                            1))))
               (setf last-atom (substring molecule last-atom-offset)
                     molecule (substring molecule 0 last-atom-offset)
                     next *ptr*))
             (format t "~D: Rn4 mol is now ~S ~S~%" *count* molecule last-atom)
             (return-from loop-inner))

             (when (equal atom "Ar")
             (let ((sub-start
                    (if (char= (char molecule 1) #\R)
                        3
                        4)))
               (setf molecule
                     (concatenate 'string molecule last-atom atom))
               (format t "~D: Ar mol was    ~A ~D~%" *count* molecule *ptr*)
               (setf molecule
                     (concatenate 'string
                                  (substring molecule 0 sub-start)
                                  (do-simple-replacements (substring molecule sub-start))))
               (setf *ptr* next)
               (let ((foo (find molecule *RnAr-replacements* :key #'car :test #'equal)))
               (format t "~D: Ar mol is now ~A ~S~%" *count* molecule foo)
                 (unless foo
                   (format t "----- no repl for ~A~%" molecule)
                   (return-from rdp molecule)))
               (assert (find molecule *RnAr-replacements* :key #'car :test #'equal))
               (decf *count*)
               (return-from rdp (cadr (find molecule
                                            *RnAr-replacements* 
                                            :key #'car :test #'equal)))))

           ;; Default case
           (setf molecule (concatenate 'string molecule last-atom)
                 last-atom atom
                 *ptr* next)))

     finally
       (decf *count*)
       (return-from rdp (do-simple-replacements molecule))))



(defun do-simple-replacements (medicine)
  (let ((global-change nil))
    (loop
       with changed = t
       while changed
       do
         (setf changed nil)
         (loop
            for (rhs lhs) in *atom-replacements*
          do
              (loop
                 for start = (search rhs medicine) ; :from-end t)
                 while start
                 do
                   ;;(incf *count*)
                   (setf changed t global-change t)
                   (format t "@~3D   ~A => ~A  ~A~%" start lhs rhs medicine)
                   (setf medicine (replace-substring
                                   lhs
                                   medicine
                                   start
                                   (length rhs))))))
    (values medicine global-change)))

(defun separate-replacements (replacements)
  (loop
     for (rhs lhs) in replacements      ; reversed
     do
       (if (and (> (length rhs) 4)
                (or
                 (and (lower-case-p (char rhs 1))
                      (equal (subseq rhs 2 4) "Rn"))
                 (equal (subseq rhs 1 3) "Rn")))
           (push (list rhs lhs) *RnAr-replacements*)
           (push (list rhs lhs) *atom-replacements*))))

(defun replace-substring (source target &optional (start 0) (len (length source)))
  "Replace len characters at the start point in the target with the
full source string."
  (concatenate 'string
               (subseq target 0 start)
               source
               (subseq target (+ start len))))

(defun reverse-replacements (replacements)
  "Given the list of lists from below, with each list being an atom
and its possible replacements, reverse it so we have a replacement
followed by its starting atom. This assumes each replacement is
unique, so we won't get clashes."
  (loop
     for repls in replacements
     append
       (loop
          with atom = (car repls)
          for repl in (cdr repls)
          collect (list repl atom))))

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

