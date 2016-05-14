;;;; Advent of code, day 19 problem 2.
;;;; Answer is ?

(defconstant *input-file* "input42.txt"
  "Where we read the strings.")

(defparameter *replacements* '())
(defparameter *count* 0)
(defparameter *memo* (make-hash-table :test 'equal))

(defun main ()
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)
    (format t "med: ~A~%repl: ~A~%~%" medicine replacements)

    (setf *replacements* (reverse-replacements replacements))

    (generate medicine '())))

(defun generate (medicine changes)
  (incf *count*)

  (when (equal medicine "e")
    (format t "Done, ~D steps: ~A~%" (length changes) changes)
    (return-from generate))

  (loop
     for (rhs lhs) in *replacements*      ; items are reversed
     for i from 1
     do
       ;; Try some memoisation.
       ;; (let ((key (list (cons lhs rhs) medicine)))
       ;;   (when (gethash key *memo*)
       ;;     ;;(format t "Short circuiting ~A => ~A in ~A~%" lhs rhs medicine)
       ;;     (return-from generate))
       ;;   (setf (gethash key *memo*) t))

       ;;(format t " grok ~A => ~A med ~A~%" lhs rhs medicine)
       (loop
          for start = (search rhs medicine)
              then (search rhs medicine :start2 (1+ start))
          while start
          do
            ;;(when (= (mod *count* 10000) 0)
              ;;(when (= (length changes) 188)
                (format t "~7d (~3D,~3D): ~2A => ~10A at ~3D, med (~3D) ~A~%"
                        *count* (length changes) i lhs rhs start (length changes) medicine);))
            (push (list lhs rhs medicine start) changes)
            (unless (and (equal lhs "e") ; replace with e should be only the
                         (not (equal rhs medicine))) ; last step
              (generate (replace-substring lhs medicine start (length rhs))
                        changes))
            (pop changes))

       finally
       (format t "bottom ~7d: med (~3D) ~A~%"
               *count* (length changes) medicine)))


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
