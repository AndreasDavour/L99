;;;; Drop every N'th element from a list.

;;; * (drop '(a b c d e f g h i k) 3)
;;; (A B D E G H K)

;;; I absolutely HATE the mixing of 0/1 indexing!!
(defun drop (lst idx)
  "Drop every IDX character from LST."
  (let ((tmp nil))
    (dotimes (x (+ 1 (length lst)))
      (if (= 0 (mod x idx))
	  nil
	  (push (nth (- x 1) lst) tmp)))
    (nreverse tmp)))
