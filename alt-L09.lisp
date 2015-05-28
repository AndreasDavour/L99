;;;; L09 - Pack consecutive duplicates of list elements into sublists.

;;; Example:
;;; * (pack '(a a a a b c c a a d e e e e))
;;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun pack (x)
  (let ((acc nil))
    (labels ((helper (y acc)
	       (cond ((equal acc nil)
		      ())
		     ((and (not (equal acc nil)) (not (equal acc (car x))))
		      ())
		     (t (format t "This should not happen!~%")))))
      (helper x acc))))


	     
	 
