;;;; Advent of code, day 21 problem 2.
;;;; Answer is 158.

(defconstant input-file "input.txt"
  "Where we read the strings.")

(defconstant weapons
  '((dagger      8 4 0)
    (shortsword 10 5 0)
    (warhammer  25 6 0)
    (longsword  40 7 0)
    (greataxe   74 8 0))
  "cost, damage, & armour. Must have one of these.")

(defconstant armours
  '((nothing       0 0 0)
    (leather      13 0 1)
    (chainmail    31 0 2)
    (splintmail   53 0 3)
    (bandedmail   75 0 4)
    (platemail   102 0 5))
  "cost, damage, & armour. Zero or one of these.")

(defconstant rings
  '((nothing      0 0 0)
    (nothing      0 0 0)
    (damage+1    25 1 0)
    (damage+2    50 2 0)
    (damage+3   100 3 0)
    (defense+1   20 0 1)
    (defense+2   40 0 2)
    (defense+3   80 0 3))
  "cost, damage, & armour. Zero to two of these, no dups.")

;;; Store stats for each player
(defclass stats ()
  ((id
    :initarg :id)
   (hit-points
    :initarg :hp
    :initform 0)
   (damage-points
    :initarg :dp
    :initform 0)
   (armour-points
    :initarg :ap
    :initform 0)))

(defmethod print-object ((inst stats) stream)
  "Print an instance of player stats."
  (format stream "~6S: hit ~a, damage ~a, armour ~a"
          (slot-value inst 'id)
          (slot-value inst 'hit-points)
          (slot-value inst 'damage-points)
          (slot-value inst 'armour-points)))
  
(defun main ()
  "Start point. Load the input and do a search."
  (play (fetch-input input-file)))

(defun play (hits-damage-armour)
  "Try all the options, and play each game."
  ;; There will be a small bit of duplicate testing here, but not
  ;; really enough to matter.
  (let ((most-cost 0))
    (loop
       for weapon in weapons
       do
         (loop
            for armour in armours
            do
              (loop
                 for i from 0 to (1- (length rings))
                 for ring1 = (nth i rings)
                 do
                   (loop
                      for j from (1+ i) to (1- (length rings))
                      for ring2 = (nth j rings)
                      do
                        (let ((boss (make-instance 'stats
                                                 :id 'boss
                                                 :hp (car hits-damage-armour)
                                                 :dp (cadr hits-damage-armour)
                                                 :ap (caddr hits-damage-armour)))
                              (player (make-instance 'stats
                                                     :id 'player
                                                     :hp 100
                                                     :dp (+ (caddr weapon)
                                                            (caddr ring1))
                                                     :ap (+ (cadddr armour)
                                                            (cadddr ring1)))))
                          (unless (eql ring1 ring2)
                            (incf (slot-value player 'damage-points)
                                  (caddr ring2))
                            (incf (slot-value player 'armour-points)
                                  (cadddr ring2)))
                          
                          (format t "~%weapon ~a~%armour ~a~%ring1  ~a~%ring2  ~a~%"
                                  weapon armour ring1 ring2)
                          (format t "~A~%" player)
                          (format t "~A~%" boss)

                          (play-game player boss)
                          (when (<= (slot-value player 'hit-points)
                                   0)
                            (let ((cost (+ (cadr weapon)
                                           (cadr armour)
                                           (cadr ring1)
                                           (cadr ring2))))
                              (format t "Player lost with cost ~a.~%" cost)
                              (when (> cost most-cost)
                                (setf most-cost cost)
                                (format t "* This is a most cost.~%")))))))))
    (format t "~%Most cost is ~A.~%" most-cost)))

(defun play-game (one other)
  "Alternate turns until one player runs out of hit points."
  (loop
     for p = one then o
     for o = other then tmp
     for tmp = p
     while (> (play-turn p o) 0)
     ;;do
       ;;(format t "~a~%" o)
     finally
       (format t "~s is out of hp, ~s has ~a~%"
               (slot-value o 'id)
               (slot-value p 'id)
               (slot-value p 'hit-points))))

(defun play-turn (player other)
  "Calculate score after player's move. Updates stats, and returns
remaining hit points for the other player."
  (decf (slot-value other 'hit-points)
        (max 1
             (- (slot-value player 'damage-points)
                (slot-value other 'armour-points)))))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns three values:
hitpoints, damage, and armour."
  (with-open-file
      (f (make-pathname :name from-file)
         :direction :input
         :if-does-not-exist :error)

    (loop
       for line = (read-line f nil)
       while line
       collect
         (parse-line line))))

;;; This gaily assumes all input is in the format:
;;; foo or more: 99
(defun parse-line (line)
  "Parse a line of input. Return just the number."

  (with-input-from-string (s line)

    ;; The lisp reader would do silly things on a raw colon, so make it
    ;; a delimiter which is otherwise ignored.
    (let ((*readtable* (copy-readtable)))
      (flet ((ignore-char (stream char)
               (declare (ignore char))
               (read stream nil nil t)))
        (set-macro-character #\: #'ignore-char)

        (loop
           for prior = item
           for item = (read s nil)
           unless item return prior)))))
