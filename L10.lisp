
;; (compress '(a a a a b c c a a d e e e e))
;; 
;; ((a a a a) (b) (c c) (a a) (d) (e e e e))

(defun compress (lst)
  (mapcar #'(lambda (x)
	      (list (car x) (length x)))
	  lst))
