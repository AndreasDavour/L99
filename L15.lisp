;;;; Replicate the elements of a list a given number of times.
;;; Example:
;;;  * (repli '(a b c) 3)
;;; (A A A B B B C C C)

;; (defun repli (lst multiplier)
;;   (loop
;;     for p in lst
;;     do (loop for i from 1 to multiplier
;; 	     do (print p))))
;; 
;; (defun my-dotimes (thing times)
;;   (labels ((again (what repeat)
;; 	     (if (= repeat 0)
;; 		 nil
;; 		 (progn
;; 		   what
;; 		   (again what (- repeat 1))))))
;;     (again thing times)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; added a package, in order to try profiling

(defpackage :L99
  (:use "COMMON-LISP")
  (:export :repli))

(in-package :L99)

(defun my-dotimes (thing times)
  (again thing times))

(defun again (thing times)
  (if (= times 0)
      nil
      (progn
	(cons thing (again thing (- times 1))))))

(defun repli (lst multiplier)
  (let ((tmp nil))
    (dolist (x lst)
      (setf tmp (append (my-dotimes x multiplier) tmp)))
    (nreverse tmp)))

