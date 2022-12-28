;;;; Advent of code, day 7 problem 1.
;;;; Answer is 1447046.

(defconstant +input-file+ "input.txt" "Where we read the goods.")

(defparameter *current-directory* nil)
(defparameter *current-inum* -1)
(defparameter *directory-total* 0)
(defparameter *inodes* (make-array 512
                                   :element-type 'inode
                                   ;;:initial-element (make-instance 'inode)
                                   :fill-pointer 0))

(defclass inode ()
  ((inum
    :initarg :inum
    :initform -1)
   (size
    :initarg :size
    :initform -1)
   (dir
    :initarg :dir
    :initform nil)))

(defclass dirent ()
  ((inum
    :initarg :inum
    :initform -1)
   (name
    :initarg :name)))
    
(defmethod print-object ((inst dirent) stream)
  "Print an instance of dirent."
  (format stream "<~3d: \"~a\">"
          (slot-value inst 'inum)
          (slot-value inst 'name)))

(defmethod print-object ((inst inode) stream)
  "Print an instance of inode."
  (format stream "<~3d: ~6D ~a>"
          (slot-value inst 'inum)
          (slot-value inst 'size)
          (if (eql nil (slot-value inst 'dir))
              "--"
              (slot-value inst 'dir))))

(defun process-command (stream)
  (ecase (read stream t nil t)
    (cd (let ((new-dir (read-line stream t nil t))) ;; plain read will barf on .. with "too many dots"
          (if (equal *current-directory* nil) ; cd /, right at start
              (progn
                (vector-push (make-instance 'inode :inum 0) *inodes*)
                (setf *current-directory* (new-directory 0)
                      *current-inum* 1))
              (progn
                (setf (slot-value
                       (aref *inodes* (slot-value (car *current-directory*) 'inum))
                       'dir)
                      *current-directory*)
                (if (equal new-dir " ..")
                    (setf *current-directory*
                          (slot-value (aref *inodes* (slot-value (cadr *current-directory*) 'inum))
                                      'dir))
                    (setf *current-directory*
                          (new-directory (slot-value (car *current-directory*) 'inum)
                                         (slot-value (find (read-from-string new-dir)
                                                           *current-directory*
                                                           :test #'equal
                                                           :key (lambda (dir)
                                                                  (slot-value dir 'name)))
                                                     'inum))))))))
    (ls '()))
  (read stream t nil t))                ; read newline

(defun new-directory (parent-inum &optional (this-inum parent-inum))
  "Return a list of the first two entries in a directory."
  (list (make-instance 'dirent
                       :inum this-inum
                       :name ".")
        (make-instance 'dirent
                       :inum parent-inum
                       :name "..")))

(defun show-directory-tree (inum depth name)
  (format t "~v@A ~A (dir)~%" depth "-" name)
  (incf depth 2)
  (loop
     for entry in (slot-value (aref *inodes* inum) 'dir)
     do
       (if (>= (slot-value (aref *inodes* (slot-value entry 'inum))
                             'size)
                 0)
         (format t "~v@A ~A (file, size=~A)~%" depth "-" (slot-value entry 'name)
                 (slot-value (aref *inodes* (slot-value entry 'inum))
                             'size))
         (when (equal 'symbol (type-of (slot-value entry 'name))) ; ignore . .. strings
           (show-directory-tree (slot-value entry 'inum)
                                depth
                                (slot-value entry 'name))))))

(defun sum-directories (path inum)
  (loop
     with dir-total = 0
     for entry in (slot-value (aref *inodes* inum) 'dir)
     do
       (if (>= (slot-value (aref *inodes* (slot-value entry 'inum))
                             'size)
                 0)
           (incf dir-total (slot-value (aref *inodes* (slot-value entry 'inum))
                                       'size))
           (when (equal 'symbol (type-of (slot-value entry 'name))) ; ignore . .. strings
             (incf dir-total (sum-directories (append (list path)
                                                      (slot-value entry 'name))
                                              (slot-value entry 'inum)))))
     finally
       (when (<= dir-total 100000)
         (incf *directory-total* dir-total)
         (format t "~A ~A -> total ~A~%" path dir-total *directory-total*))
       (return dir-total)))
     

(defun main ()
  (with-open-file
      (f (make-pathname :name +input-file+)
         :direction :input
         :if-does-not-exist :error)

    ;; Make our own copy of the readtable, so we can change it
    (let ((*readtable* (copy-readtable)))

      ;; $ signals the start of a command
      (set-macro-character
       #\$ (lambda (stream char)
             (declare (ignore char))
             (process-command stream)))
      
      (loop
         for size = (read f nil)
         for name = (read f nil)
         while size
         do
           (vector-push-extend (make-instance 'inode
                                              :inum *current-inum*
                                              :size (if (not (equal (type-of size) 'symbol))
                                                        size
                                                        -1))
                               *inodes*)
           (setf *current-directory*
                 (append *current-directory*
                         (list (make-instance 'dirent
                                        :inum *current-inum*
                                        :name name))))
           (incf *current-inum*)
         finally
           (setf (slot-value
                  (aref *inodes* (slot-value (car *current-directory*) 'inum))
                  'dir)
                 *current-directory*)
           (show-directory-tree 0 1 "/")
           (sum-directories (list "/") 0)))))
           
