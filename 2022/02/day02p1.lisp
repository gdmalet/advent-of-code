;;;; Advent of code, day 2 problem 1.
;;;; Answer is 12535.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun round-score (opponent player)
  "What the player scores for each round"
  (ecase opponent
	(#\A (ecase player
		   (#\X 4)		; draw 1 + 3
		   (#\Y 8)		; win  2 + 6
		   (#\Z 3)))	; lose 3 + 0
	(#\B (ecase player
		   (#\X 1)		; lose 1 + 0
		   (#\Y 5)		; draw 2 + 3
		   (#\Z 9)))	; win  3 + 6
	(#\C (ecase player
		   (#\X 7)		; win  1 + 6
		   (#\Y 2)		; lose 2 + 0
		   (#\Z 6)))))	; draw 3 + 3
		
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
	 
