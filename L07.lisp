;;;; L07 - Flatten a nested list structure.

(defun my-flatten (lst)
  (let ((tmp nil))
    (labels ((recurse-over-list (lst2)
	       (cond
		 ((atom (car lst2))
		  (format t "atom~%")
		  (push (car lst2) tmp)
		  (recurse-over-list (cdr lst2)))
		 ((listp (car lst2))
		  (format t "list")
		  (recurse-over-list (car lst2)))
		 (nil nil))))
      (recurse-over-list lst))
    tmp))

(defun my-flatten (lst)
  (let ((tmp nil))
    (labels ((recurse-over-list (lst2)
	       (if (atom (car lst2))
		   (progn
		     (format t "atom~%")
		     (push (car lst2) tmp)
		     (recurse-over-list (cdr lst2))))
	       (if (listp (car lst2))
		   (progn
		     (format t "list")
		     (recurse-over-list (car lst2))))
	       (if (equal (cdr lst2) nil)
		   nil)))
      (recurse-over-list lst))
    tmp))

(defparameter tmp nil)

(defun my-flatten (lst)
  (if (null (car lst))
	nil
	(progn
	  (push-atoms lst)
	  (print tmp)
	  (my-flatten (cdr lst)))))

(defun push-atoms (lst2)
  (format t "lst2:~A collector:~A~%" lst2 tmp)
  (if (zerop (car lst2))
      nil
      (push (car lst2) tmp)))


(defun flatten-helper (lst2)
    (if (null (car lst2))
	nil
	(cond
	  ((atom (car lst2))
	   (push (car lst2) tmp)
	   (flatten-helper (cdr lst2)))
	  ((listp (car lst2))
	   (append (car lst2) tmp)))))

(defun my-flatten (lst)
  (let ((tmp nil))
    (flatten-helper lst)
    tmp))


