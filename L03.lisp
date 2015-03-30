;;;; L03 - n'th element of list

(defun element (e lst)
  (do ((counter 0 (+ counter 1)))
      ((> counter e))
    

    (format t "~A (~A) " counter lst)))

;; 
(defun elements (e lst)
  (if (< e 2)
      (car lst)
      (elements (- e 1) (cdr lst))))
