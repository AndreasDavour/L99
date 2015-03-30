;;;; L01 - last box of list

(defun mylast (x)
  (car (nreverse x)))

(defun mylast (x)
  (if (null (cdr x))
      (car x)
      (mylast (cdr x))))
