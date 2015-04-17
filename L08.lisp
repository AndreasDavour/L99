;;;; L08 - Eliminate consecutive duplicates of list elements.

;;; recursion with accumulator
(defun my-remove-duplicates (original-lst)
  (labels ((duplicates-rec (lst acc)
	     (cond
	       ((null lst)                       (nreverse acc))
	       ((eql (car acc) (car lst))        (duplicates-rec (cdr lst) acc))
	       ((not (eql (last acc) (car lst))) (duplicates-rec (cdr lst) (cons (car lst) acc))))))
    (duplicates-rec original-lst nil)))

