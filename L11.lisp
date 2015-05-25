;; P11 (*) Modified run-length encoding.
;; 
;; Modify the result of problem P10 in such a way that if an element has
;; no duplicates it is simply copied into the result list. Only elements
;; with duplicates are transferred as (N E) lists.
;; 
;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

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

(defun encode (lst)
  "Frequency compacts elements of a list"
  (let* ((packed-list (pack lst))
	 (slimlist nil))
    ;;; UGLY post processing hack! PACK should be refactored!
    ;; make sublists except for the singles
    (dolist (tmp packed-list)
      (if (equal (length tmp) 1)
	  (push (car tmp) slimlist)
	  (push tmp slimlist)))
    ;; now count them, unless atoms
    (mapcar #'(lambda (x)
		(if (atom x)
		    x
		    (list (length x) (car x))))
	    (nreverse slimlist))))

