;;;; Advent of code, day 22 problem 1.
;;;; Answer is .

(defconstant input-file "input.txt"
  "Where we read the strings.")

(defclass spell ()
  ((id     :initarg :id     :reader get-id)
   (cost   :initarg :cost   :reader get-cost)
   (damage :initarg :damage :initform 0 :reader get-damage)
   (armour :initarg :armour :initform 0 :reader get-armour)
   (heals  :initarg :heals  :initform 0 :reader get-heals)
   (mana   :initarg :mana   :initform 0 :reader get-mana)
   (timer  :initarg :timer  :initform 0 :reader get-timer)))

(defmethod print-object ((inst spell) stream)
  "Print an instance of a spell."
  (format stream "~13S: cost ~3,' d, damage ~a, armour ~a, heals ~a, mana ~3,' d, timer ~a"
          (get-id inst)
          (get-cost inst)
          (get-damage inst)
          (get-armour inst)
          (get-heals inst)
          (get-mana inst)
          (get-timer inst)))

(defparameter spells
  (list (make-instance 'spell :id 'magic-missile :cost  53 :damage 4)
        (make-instance 'spell :id 'drain         :cost  73 :damage 2 :heals 2)
        (make-instance 'spell :id 'shield        :cost 113 :armour 7 :timer 6)
        (make-instance 'spell :id 'poison        :cost 173 :damage 3 :timer 6)
        (make-instance 'spell :id 'recharge      :cost 229 :mana 101 :timer 5)))

;;; Store stats for each player
(defclass stats ()
  ((id            :initarg :id)
   (hit-points    :initarg :hp
                  :initform 0)
   (damage-points :initarg :dp
                  :initform 0)
   (mana          :initarg :mana
                  :initform 0)
   (spells        :accessor spells
                  :initform nil)))

(defmethod print-object ((inst stats) stream)
  "Print an instance of player stats."
  (format stream "~6S: hit ~a, damage ~a, mana ~a, spells:~%~a"
          (slot-value inst 'id)
          (slot-value inst 'hit-points)
          (slot-value inst 'damage-points)
          (slot-value inst 'mana)
          (slot-value inst 'spells)))
  
(defun main ()
  "Start point. Load the input and do a search."
  (play (fetch-input input-file)))

(defun play (hits-damage)
  "Try all the options, and play each game."
  ;; There will be a small bit of duplicate testing here, but not
  ;; really enough to matter.
  (let ((least-cost 999999))

    (let ((boss (make-instance 'stats :id 'boss
                               :hp (car hits-damage)
                               :dp (cadr hits-damage)))
          
          (player (make-instance 'stats :id 'player
                                 :hp 50
                                 :mana 500)))

      (format t "~A~%" player)
      (format t "~A~%" boss)

      (play-game player boss)

                          (when (> (slot-value player 'hit-points)
                                   0)
                            (let ((cost (+ (cadr weapon)
                                           (cadr armour)
                                           (cadr ring1)
                                           (cadr ring2))))
                              (format t "Player won with cost ~a.~%" cost)
                              (when (< cost least-cost)
                                (setf least-cost cost)
                                (format t "* This is a least cost.~%"))))))
    (format t "~%Least cost is ~A.~%" least-cost))

(defun play-game (one other)
  "Alternate turns until one player runs out of hit points, or the
player runs out of mana."
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
             (- (+ (slot-value player 'damage-points)
                   (loop
                      for sp in (spells player)
                      summing (get-damage sp) into damage-points
                      finally (return damage-points)))
                (loop
                   for sp in (spells player)
                   summing (get-armour sp) into armour-points
                   finally (return armour-points))))))

(defun fetch-input (from-file)
  "Fetch all the input from the file. Returns two values:
hitpoints & damage."
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
