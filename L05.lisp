;;;; L05 - reverse a list

(defun my-reverse (lst)
  (let ((counter (- (length lst) 1)))
    (labels ((helper-recursor (lst2 count)
	       (if (< count 0)
		   nil
		   (cons (nth count lst2) (helper-recursor lst2 (decf count))))))
      (helper-recursor lst counter))))
