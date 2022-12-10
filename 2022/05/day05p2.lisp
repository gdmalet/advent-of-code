;;;; Advent of code, day 5 problem 2.
;;;; Answer is TZLTLWRNF.

(defconstant +input-file+ "input.txt"
  "Where we read the goods.")

(defun process-moves (stacks line)
  "Process moves in a line of this format:
move 5 from 3 to 6"
	(loop
	  with count = (read-from-string line nil nil :start 5)
	  with from = (1- (read-from-string line nil nil :start 12))
	  with to = (1- (read-from-string line nil nil :start 17))
	  with crates = 'nil
	  while (>= (decf count) 0)
	  do
		 ;;(format t "\"~A\" count ~A, from ~A, to ~A~%" line count from to)
		 (setf crates (append crates (list (pop (aref stacks from)))))
	  finally
		 (setf (aref stacks to) (append crates (aref stacks to)))
		 (return stacks)))
  
  (format t "STACKS ~A~%" stacks))

(defun main ()
  (with-open-file
	  (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

	 ;; First loop to read the stacks
	 ;; [M]                     [N] [Z]    
	 ;; [F]             [R] [Z] [C] [C]    
	 ;; [C]     [V]     [L] [N] [G] [V]    
	 ;; [W]     [L]     [T] [H] [V] [F] [H]
	 ;; [T]     [T] [W] [F] [B] [P] [J] [L]
	 ;; [D] [L] [H] [J] [C] [G] [S] [R] [M]
	 ;; [L] [B] [C] [P] [S] [D] [M] [Q] [P]
	 ;; [B] [N] [J] [S] [Z] [W] [F] [W] [R]
     ;;  1   2   3   4   5   6   7   8   9

	(let ((stacks (make-array 9 :initial-element nil)))
	  (loop
		for line = (read-line f nil)
		while (char= (schar line 0) #\[) ; only while reading stacks
		do
		   (loop
			 for i from 0 to 8
			 for crate = (schar line (1+ (* i 4)))
			 when (not (char= crate #\space))
			   do
				  (setf (aref stacks i) (append (aref stacks i) (list crate)))))

	  (format t "Stacks before moves: ~A~%" stacks)

	  (read-line f nil)					; skip blank line

	  ;; Process the moves
	  (loop
		for line = (read-line f nil)
		while line
		do
		   (process-moves stacks line))

	  (format t "Stacks after moves: ~A~%" stacks)

	  ;; Show the top of each stack
	  (format t "Top of stacks: ")
	  (loop for list across stacks
			do (format t "~A" (car list)))
	  (format t "~%"))))

		   
