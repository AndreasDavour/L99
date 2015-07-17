;;;; L18 - Extract a slice from a list.

;;; Given two indices, I and K, the slice is the list containing the
;;; elements between the I'th and K'th element of the original list (both
;;; limits included). Start counting the elements with 1.
;;; 
;;; Example:
;;; * (slice '(a b c d e f g h i k) 3 7)
;;; (C D E F G)

(defun slice (lst start stop)
  (loop 
    for p from (- start 1) to (- stop 1)
    collect (nth p lst)))

;;;; (slice '(a b c d e f g h i k) 3 7)

