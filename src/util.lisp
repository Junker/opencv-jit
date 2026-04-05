(defpackage opencv-jit/util
  (:use #:cl
        #:cl-annot)
  (:documentation "Utility functions for opencv-jit."))
(in-package :opencv-jit/util)

(cl-annot:enable-annot-syntax)

@export
(defun const-kw-int (kw alist)
  "Look up KW in ALIST and return the associated integer value."
  (cdr (assoc kw alist)))

@export
(defun const-int-kw (int alist)
  "Look up INT in ALIST and return the associated keyword."
  (car (rassoc int alist)))

@export
(defmacro defconstant-exp (name val)
  "Define and export a constant with NAME and value VAL."
  `(progn
     (export (quote ,name))
     (defconstant ,name ,val)))
