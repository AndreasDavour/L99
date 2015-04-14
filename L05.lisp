;;;; L05 - reverse a list


;;; infinite loop - needs tweaking
(defun my-reverse (lst)
  (let ((mylst nil))
    (labels ((rev (x)
	       (if (null (cdr lst))
		   mylst
		   (progn
		     (push (car lst) mylst)
		     (rev (cdr x))))))
      (rev lst))))

(defun my-reverse (lst)
  (if (equal nil lst)
      nil
      (cons (car lst) (my-reverse (cdr lst)))))
