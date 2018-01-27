;;;; Advent of code, day 19 problem 2.
;;;; Answer is 207

(defconstant *input-file* "input.txt"
  "Where we read the strings.")

;;; Using the formula from
;;; https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
(defun get-count ()
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)
    (loop
       with Ar-count = 0 and Rn-count = 0 and Y-count = 0 and atom-count = 0
       for index = 0 then (+ index (length atom))
       for atom = (get-next-atom medicine index)
       while atom
       do
         (incf atom-count)
         (cond ((equal atom "Rn") (incf Rn-count))
               ((equal atom "Ar") (incf Ar-count))
               ((equal atom "Y")  (incf Y-count)))
         (format t "atom ~S,~10T Rn ~D, Ar ~D, Y ~D, count ~D, index ~D~%"
                 atom Rn-count Ar-count Y-count atom-count index)
       finally (return (- atom-count
                          1
                          (+ Ar-count Rn-count)
                          (* 2 Y-count))))))

;;;; ---- Attempted solution below. It doesn't work :-(
;;;; This gets stuck with string "TiRnPMgAr", which should ->
;;;; TiRnFAr -> B...

(defparameter *atom-replacements* '())
(defparameter *RnAr-replacements* '())
(defparameter *count* 0)
(defparameter *ptr* 0)
(defparameter *medicine* "")

(defun main ()
  (multiple-value-bind (medicine replacements)
      (fetch-input *input-file*)
    (format t "medicine (~D):~%~A~%~%replacements (~D): ~A~%~%"
            (length medicine) medicine (length replacements) replacements)

    (setf *medicine* medicine *atom-replacements* nil *RnAr-replacements* nil)
    (separate-replacements (reverse-replacements replacements))

    ;;(setf *atom-replacements* (nreverse *atom-replacements*))

    (format t "atom-replacements (~D): ~A~%~%RnAr-replacements (~D): ~A~%~%"
            (length *atom-replacements*) *atom-replacements* 
            (length *RnAr-replacements*) *RnAr-replacements*)

    (setf *count* 0)
    (setf *ptr* 0)

    (do-simple-replacements 0 t)))

    ;; (let* ((first (get-next-atom *medicine* 0))
    ;;       (second (get-next-atom *medicine* (+ (length first)))))
    ;;   (rdp 0 first second))))
          

(defun rdp2 (offset first second)
  "Recursive descent parser."
  (format t "Entering rdp2, offset ~A, ~A ~A~%" offset first second)
  (loop
     for last = second then next
     for index = (+ offset (length first) (length second)) then (+ index (length last))
     for next = (get-next-atom *medicine* index)
     while next
     do
       (format t "index ~D, last ~a, next ~a~%" index last next)
       (cond
         ((equal next "Rn")
          (princ "...recursing ... ")
          (rdp2 (- index (length last)) last next)
          ;;;reset index & continue?
          (setf index (- index (length last))
                next (get-next-atom *medicine* index))
          (format t "RESET: index ~D, last ~a, next ~a~%" index last next))
         ((equal next "Ar")
          (format t "string is \"~A\"~%" (subseq *medicine* offset (+ index 2)))
          (loop
             for (rhs lhs) in *RnAr-replacements*
             do
            (format t "checking ~a => ~a~%" lhs rhs)
            (when (equal rhs (subseq *medicine* offset (+ index 2)))
              (format t "replacing ~A at ~D with ~A in~%>~A~%" rhs offset lhs *medicine*)
              (setf *medicine* (replace-substring lhs *medicine* offset (length rhs)))
              (format t "<~A~%" *medicine*)
              (format t " 01234567890123456789012345678901234567890123456789~%")
              (return-from rdp2)))))))
  

;;; Start of medicine string assumed to be XRn.
;;; Look for ending Ar, or recurse if we hit another Rn.
(defun rdp (offset first second)
  "Recursive descent parser."
  (format t "Entering rdp, offset ~A, ~A ~A~%" offset first second)
  (loop
     for last = second then next
     for index = (+ offset (length first) (length second)) then (+ index (length last))
     for next = (get-next-atom *medicine* index)
     do
       (format t "last ~a, index ~A, next ~a~%" last index next)
       (cond
         ((equal next "Rn")
          (princ "...recursing ... ")
          (rdp (- index (length last)) last next)
          ;;;reset index & continue?
          (return-from rdp))
         ((equal next "Ar")
          (format t "string is \"~A\"~%" (subseq *medicine* offset (+ index 2)))
          (loop
             for (rhs lhs) in *RnAr-replacements*
             do
            (format t "checking ~a => ~a~%" lhs rhs)
            (when (equal rhs (subseq *medicine* offset (+ index 2)))
              (format t "replacing ~A at ~D with ~A in~%>~A~%" rhs offset lhs *medicine*)
              (setf *medicine* (replace-substring lhs *medicine* offset (length rhs)))
              (format t "<~A~%" *medicine*)
              (format t " 01234567890123456789012345678901234567890123456789~%")
              (return-from rdp))))
         ((not (equal last "RnXX"))
          (format t "??1 index ~D, offset ~D, last ~A, next ~A~%"
                  index offset last next)
          (do-simple-replacements index)
          (setf next (get-next-atom *medicine* index)))
         (t
          (format t "??2 index ~D, offset ~D, last ~A, next ~A~%"
                  index offset last next)))))
  


(defun do-simple-replacements (start &optional (recurse nil))
  "Make replacements in the given medicine until we hit a longer
molecule, then call the parser to deal with it..."
  (format t "Entering do-simple-replacements, start at ~D~%" start)
  (loop
     for first = (get-next-atom *medicine* start)
     for second = (get-next-atom *medicine* (+ start (length first)))
     do
       (block continue
         (format t "Got first ~A, second ~A at start ~D~%" first second start)

         (cond ((and recurse (equal second "Rn"))
                (rdp2 start first second)
                (format t "back from rdp~%")
                (return-from continue))
               (t
                (loop
                   for (rhs lhs) in *atom-replacements*
                   do
                     ;;(format t "checking ~a => ~a~%" lhs rhs)
                     (when (equal rhs (concatenate 'string first second))
                       (format t "replacing ~A at ~D with ~A in~%>~A~%" rhs start lhs *medicine*)
                       (setf *medicine* (replace-substring lhs *medicine* start (length rhs)))
                       (format t "<~A~%" *medicine*)
                       (format t " 01234567890123456789012345678901234567890123456789~%")
                       (return-from continue)))
                (format t "Exiting do-simple-replacements~%")  ; no replacements
                (return-from do-simple-replacements))))))

(defun separate-replacements (replacements)
  "Separate replacements into atoms or RnAr replacements."
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
  (incf *count*)
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

