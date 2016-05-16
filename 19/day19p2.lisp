;;;; Advent of code, day 19 problem 2.
;;;; Answer is ?

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

(defparameter *atom-replacements* '())
(defparameter *RnAr-replacements* '())
(defparameter *count* 0)

(defun main ()
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)
    (format t "med: ~A~%repl: ~A~%~%" medicine replacements)

    (setf *atom-replacements* nil *RnAr-replacements* nil)
    (separate-replacements (reverse-replacements replacements))
    (setf *count* 0)

    (generate medicine '())))

(defun generate (medicine changes)
  (incf *count*)
  ;;(when (> *count* 100000)
  ;;  (return-from generate))

  (setf medicine (do-simple-replacements medicine changes))
  ;;(format t " ---- Simple atoms done: ~A~%" medicine)

  (loop
     with changed = t
     while changed
     do
       (setf changed nil)
       (loop
          with next-start = 0
          for start = (search "Rn" medicine :start2 next-start)
          while (and start
                     (< start (length medicine)))
          do
            (let* ((left-offset (if (lower-case-p (char medicine (1- start)))
                                    2
                                    1))
                   (end (search "Ar" medicine :start2 (+ start 2)))
                   (start (- start left-offset))
                   (len (+ (- end start) 2))
                   (rhs (subseq medicine start (+ start len)))
                   (lhs (cadr (find rhs *replacements* :key #'car :test #'equal))))
              (when lhs
                (format t " ~3D:~D ~A => ~A  ~A~%" start len lhs rhs medicine)
                (setf changed t)
                (setf medicine (replace-substring
                                lhs
                                medicine
                                start
                                len))
                (setf next-start 0))
              (unless lhs
                (setf next-start (+ start left-offset 1)))))
       (unless changed
         (multiple-value-bind (med chg)
             (do-simple-replacements medicine changes)
           (setf medicine med changed chg)))))


(defun do-simple-replacements (medicine changes)
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
                 for start = (search rhs medicine)
                 while start
                 do
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
