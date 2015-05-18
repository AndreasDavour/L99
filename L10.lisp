
;; (compress '(a a a a b c c a a d e e e e))
;; 
;; ((a a a a) (b) (c c) (a a) (d) (e e e e))

;; dirty solution from L09
(defun pack (lst)
  "Correctly packs repeated symbols into sublists."
  (let ((latest (car lst))
	(acc nil)
	(result nil)
	(counter (length lst)))
    (mapcar #'(lambda (x)
		(setf counter (- counter 1))
		(if (equal 0 counter)
		    (progn
		      (setf acc (cons x acc))
		      (setf result (cons acc result)))
		    (if (equal x latest)
			(progn
;;			  (format t "equal, x: ~A acc: ~A latest: ~A~%" x acc latest)
			  (setf acc (cons x acc)))
			(progn
;;			  (format t "not equal, x: ~A acc: ~A latest: ~A~%" x acc latest)
			  (setf result (cons acc result))
			  (setf latest x)
			  (setf acc nil)
			  (setf acc (cons x acc))))))
	    lst)
    (nreverse result)))

(defun compress (lst)
  (mapcar #'(lambda (x)
	      (list (length x) (car x)))
	  (pack lst)))
