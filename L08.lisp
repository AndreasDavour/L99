;;;; L08 - Eliminate consecutive duplicates of list elements.

;;; Example:
;;; * (compress '(a a a a b c c a a d e e e e))
;;; (A B C A D E)

;; iterative solution
(defun compress (x)
  (let ((uniq-chars nil))
    (loop for i in x
	  do (if (not (member i uniq-chars))
		 (push i uniq-chars)))
    (nreverse uniq-chars)))

;;; recursion with accumulator
(defun my-remove-duplicates (original-lst)
  (labels ((duplicates-rec (lst acc)
	     (cond
	       ((null lst)                       (nreverse acc))
	       ((eql (car acc) (car lst))        (duplicates-rec (cdr lst) acc))
	       ((not (eql (last acc) (car lst))) (duplicates-rec (cdr lst) (cons (car lst) acc))))))
    (duplicates-rec original-lst nil)))
