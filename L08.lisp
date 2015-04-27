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
