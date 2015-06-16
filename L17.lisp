;;;;  Split a list into two parts, the length of the first part is given.
;;;  Do not use any predefined predicates.
;;; 
;;; Example:
;;; * (split '(a b c d e f g h i k) 3)
;;; ( (A B C) (D E F G H I K))

(defun split (lst idx)
  "Split the LST after IDX atoms."
  (cons (subseq lst 0 idx)
	(cons (subseq lst (+ idx 1)) nil)))

;; a recursive solution
;; if you want more code, doing less
(defun split (lst idx)
  "Split the LST after IDX atoms."
  (labels ((splitter (list-arg acc templist)
	     (cond
	       ((null list-arg)
		nil)
	       ((eql acc idx)
		(cons (nreverse templist) (cons list-arg nil)))
	       (t
		(push (car list-arg) templist)
		(splitter (cdr list-arg) (+ acc 1) templist)))))
    (splitter lst 0 nil)))
