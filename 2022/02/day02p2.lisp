;;;; Advent of code, day 2 problem 1.
;;;; Answer is 12535.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun round-score (opponent player)
  "What the player scores for each round"
  (ecase opponent ; rock 1 paper 2 scissors 3, lose 0 draw 3 win 6
	(#\A (ecase player
		   (#\X 3)		; lose 3 + 0
		   (#\Y 4)		; draw 1 + 3
		   (#\Z 8)))	; win  2 + 6
	(#\B (ecase player
		   (#\X 1)		; lose 1 + 0
		   (#\Y 5)		; draw 2 + 3
		   (#\Z 9)))	; win  3 + 6
	(#\C (ecase player
		   (#\X 2)		; lose 2 + 0
		   (#\Y 6)		; draw 3 + 3
		   (#\Z 7)))))	; win  1 + 6
		
(defun main ()
   (with-open-file
       (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

	 (loop
	   for opponent = (read f nil)
	   for player = (read f nil)
	   while opponent
	   summing (round-score (schar (symbol-name opponent) 0)
							(schar (symbol-name player) 0)) into total
	   finally (return total))))
	 
