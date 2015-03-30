;;; L02 - last but one

(defun last-but-one (x)
  (if (null (cdr x))
      x
      (last-but-one (cdr x))))
