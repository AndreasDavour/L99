;;; REverse of L11, decode a list of encoded symbols
;;; ((4 a) b (3 c)) -> (a a a a b c c c)

;; kind of works. Seems to drop the last sublist for some reason.
(defun decode (lst)
  (let ((tmp nil))
    (labels ((unpack (x)
	       (cond
;;;		 ((null (cdr x))
		 ((not (> (length (cdr x)) 0))
		  nil)
		 ((and (atom (car x)) (not (numberp (car x))))
;;;		 ((atom x)
		  (format t "A>> car:~A cdr:~A~%" (car x) (cdr x))
		  (push (car x) tmp)
		  (unpack (cdr x)))
		 ((listp (car x))
		  (dotimes (p (car (car x)))
		    (push (car (cdr (car x))) tmp)
		    (format t "L>> car:~A cdrcar:~A p:~A tmp:~A cdr:~A~%" (car x) (car (cdr (car x))) p tmp (cdr x)))
		  (unpack (cdr x)))
		 
		  )))
      (unpack lst))
    (nreverse tmp)))
