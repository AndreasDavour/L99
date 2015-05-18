;;;; L09 - Pack consecutive duplicates of list elements into sublists.

;;; If a list contains repeated elements they should be placed in separate sublists.
;;; 
;;; Example:
;;; * (pack '(a a a a b c c a a d e e e e))
;;;     ((A A A A) (B) (C C) (A A) (D) (E E E E))


;;; Interative solution
;;; really kludgy, needs cleanup
;;; suggestion: use COND to make the conditionals more c;ear

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



;;; ;;; NON WORKING RECURSIVE ATTEMPT
;;; ;;;
;;; ;;; how to recurse
;;; ;; this is our end state
;;; ; (cons (cons 'a (cons 'a (cons 'a (cons 'a nil)))) (cons (cons 'b nil) (cons (cons 'c (cons 'c nil)) (cons (cons 'a (cons 'a nil)) (cons (cons 'd nil) (cons 'e (cons 'e (cons 'e (cons 'e nil)))))))))
;;; 
;;; 
;;; (defun pack (x)
;;;   (let* ((tmp (car x)))
;;;     (pack-helper x tmp)))
;;; 	 
;;; 
;;; (defun pack-helper (y acc)
;;;   (cond
;;;     ;; if end of list, cdr is nil
;;;     ((equal (cdr y) nil)
;;;      nil)
;;;     ;; last car and present car is the same
;;;     ((equal acc (car y))
;;;      (cons (car y) (pack-helper (cdr y) acc)))
;;;     (t
;;;      ;; (A A A A (B) B (C) C C (A) A A (D) D (E) E E E)
;;;      ;; (cons (cons (car y) nil) (pack-helper y (car y))))))
;;;      ;; (A A A A (B) (C) C (A) A (D) (E) E E)
;;;      ;;
;;;      ;; last car and present car is not the same
;;;      (cons (cons (car y) nil) (pack-helper (cdr y) (car y))))))
