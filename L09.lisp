;;;; L09 - Pack consecutive duplicates of list elements into sublists.

;;; If a list contains repeated elements they should be placed in separate sublists.
;;; 
;;; Example:
;;; * (pack '(a a a a b c c a a d e e e e))
;;;     ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun pack (x)
  (let ((lst (car x)))
    (loop for i in x 
	  until (not (equal i lst))
	  collecting i)))

(defun pack (x)
  (let ((tmp nil)
	(last (first x)))
    (loop for i in x
	  for former = last
	  do (setf last i)
	  if (equal i former)
	    do (push i tmp)
	  else
	    do (progn 
		 (setf tmp (list tmp))
		 (push i tmp))
	  end)
    tmp))

;;; how to recurse
;; this is our end state
; (cons (cons 'a (cons 'a (cons 'a (cons 'a nil)))) (cons (cons 'b nil) (cons (cons 'c (cons 'c nil)) (cons (cons 'a (cons 'a nil)) (cons (cons 'd nil) (cons 'e (cons 'e (cons 'e (cons 'e nil)))))))))


;; (let ((tmp (car lst)))
;;   (labels ((helper (y acc) 
;; 	     (cond
;; 	       ((equal (cdr y) nil)
;; 		nil)
;; 	       ((equal tmp (car y))
;; 		(cons (car y) (helper (cdr y) acc))))))
;;     (helper lst tmp)))


(defun pack (x)
  (let* ((tmp (car x)))
    (pack-helper x tmp)))
	 

(defun pack-helper (y acc)
  (cond
    ;; if end of list, cdr is nil
    ((equal (cdr y) nil)
     nil)
    ;; last car and present car is the same
    ((equal acc (car y))
     (cons (car y) (pack-helper (cdr y) acc)))
    (t
     ;; (A A A A (B) B (C) C C (A) A A (D) D (E) E E E)
     ;; (cons (cons (car y) nil) (pack-helper y (car y))))))
     ;; (A A A A (B) (C) C (A) A (D) (E) E E)
     ;;
     ;; last car and present car is not the same
     (cons (cons (car y) nil) (pack-helper (cdr y) (car y))))))

