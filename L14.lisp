;;;; Duplicate the elements of a list.
;;; Example:
;;; * (dupli '(a b c c d))
;;; (A A B B C C C C D D)

(defun dupli (lst)
  "Cons on each element as we recurse down the list."
  (if (null lst)
      nil
      (cons (car lst) (cons (car lst) (dupli (cdr lst))))))
