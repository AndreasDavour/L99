;;;; L06 - Find out whether a list is a palindrome.

(defun palindromep (lst)
  (if (equal (reverse lst) lst)
      t
      nil))

(defun palindromep (lst)
  (and (equal (reverse lst) lst)))
