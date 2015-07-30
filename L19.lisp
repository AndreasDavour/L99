;;;; Rotate a list N places to the left.

;;; Examples:
;;; * (rotate '(a b c d e f g h) 3)
;;; (D E F G H A B C)
;;; 
;;; * (rotate '(a b c d e f g h) -2)
;;; (G H A B C D E F)

(defun rotate (lst shift)
  (let ((overflow))
    (labels ((helper-positive (data acc)
	       (if (= acc 0)
		   (setf overflow (append data (nreverse overflow)))
		   (progn
		    (push (car data) overflow)
		    (helper-positive (cdr data) (- acc 1)))))
	     (helper-negative (data acc)
	       (if (= acc 0)
		   (setf overflow (append overflow data))
		   (progn
		     (push (car (reverse data)) overflow)
		     (helper-negative (reverse (cdr (reverse data))) (- acc 1))))))
      (if (> shift 0)
	  (helper-positive lst shift)
	  (helper-negative lst (abs shift)))
      overflow)))
