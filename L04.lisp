;;;; L04 - find numbers of elements in list

(defun number-of-elements (lst)
  (let ((count 1))
    (labels ((noe (elm)
	       (if (null (cdr elm))
		   count
		   (progn
		     (incf count)
		     (noe (cdr elm))))))
      (noe lst))))


