;;;; Run-length encoding of a list (direct solution).

;;; Implement the so-called run-length encoding data compression
;;; method directly. I.e. don't explicitly create the sublists
;;; containing the duplicates, as in problem P09, but only count
;;; them. As in problem P11, simplify the result list by replacing the
;;; singleton lists (1 X) by X.
;;; 
;;;     Example:
;;;     * (encode-direct '(a a a a b c c a a d e e e e))
;;;     ((4 A) B (2 C) (2 A) D (4 E))

;; (defparameter full '(A A A A B C C C D))

;;; (encode-direct '(a a a a b c c a a d e e e e))
;;; (B (2 C) (6 A) D (4 E))

;;; seems like the repeating atoms are not handled correctly

;; (defun encode-direct (lst)
;;   (let ((tmp (remove-duplicates lst))
;; 	(acc nil))
;;     (dolist (var tmp)
;;       (if (> (length (intersection lst (list var nil))) 1)
;; 	  (push (list (length (intersection lst (list var nil))) var) acc)
;; 	  (push var acc)))
;;     (nreverse acc)))

(defun my-remove-duplicates (lst)
  (let ((acc nil))
;	(tmp nil))
;   (labels ((dup-helper ()

  (cond
    ((null lst)
     nil)
    ((not (eql (car lst) (cadr lst)))
;;     (format t "eql: ~A~%" (car lst)))
     (push (car lst) acc)
     (my-remove-duplicates (cdr lst)))
    (t
;;     (format t "t: ~A~%" (car lst))
     (my-remove-duplicates (cdr lst))
     ))
    (print acc)))

(defun encode-direct (lst)
  (let ((tmp (remove-duplicates lst))
	(acc nil))
    (dolist (var tmp)
      (if (> (length (intersection lst (list var nil))) 1)
	  (push (list (length (intersection lst (list var nil))) var) acc)
	  (push var acc)))
    (nreverse acc)))

