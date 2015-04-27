;;;; L07 - Flatten a nested list structure.

;;; Paul Graham's solution
(defun my-flatten (x)
  (labels ((rec (x acc)
	     (cond
	       ((null x) acc)
	       ((atom x) (cons x acc))
	       (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun my-flatten (x)
  (rec x nil))

(defun rec (x acc)
    (cond
      ((null x) acc)
      ((atom x) (cons x acc))
      (t (rec (car x) (rec (cdr x) acc)))))

;;; someone elses solution
(defun my-flatten (orig-list)
  (if (eql orig-list nil)
      nil
      (let ((elem (car orig-list))
	    (resto-list (cdr orig-list)))
	(if (listp elem)
	    (append (my-flatten elem) (my-flatten resto-list))
	    (append (cons elem nil) (my-flatten resto-list))))))

;;; my solution
