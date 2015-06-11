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
  "Helper function to get all uniqe values in order."
  (labels ((dup-helper (x acc)
	     (cond
	       ;; end of list
	       ((null x)
		(nreverse acc))
	       ;; new value
	       ((not (eql (car x) (cadr x)))
		(push (car x) acc)
		(dup-helper (cdr x) acc))
	       ;; same old, let's move along
	       (t
		(dup-helper (cdr x) acc)
		))))
      (dup-helper lst nil)))

(defun encode-direct (lst)
  (let ((tmp (remove-duplicates lst))
	(acc nil))
    (dolist (var tmp)
      (if (> (length (intersection lst (list var nil))) 1)
	  (push (list (length (intersection lst (list var nil))) var) acc)
	  (push var acc)))
    (nreverse acc)))

;; the problem with this is INTERSECTION takes the whole list and
;; will thus find all the A, and not just the first set.
(defun encode-direct (lst)
  (let ((tmp (my-remove-duplicates lst))
	(acc nil))
    (dolist (var lst)
  (format t "apa: ~A~%" (intersection lst (list var nil))))
;;      (if (> (length (intersection lst (list var nil))) 1)
;;	  (push (list (length (intersection lst (list var nil))) var) acc)
;;	  (push var acc)))
    (nreverse acc)))

;; find the count of each atom
;; same problem as above, though.
(loop for i in '(a c b e)
      for p = (count i '(a a a a c c c b e e e))
      do (print p))
