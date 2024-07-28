(defpackage opencv-jit/util
  (:use #:cl
        #:cl-annot))
(in-package :opencv-jit/util)

(cl-annot:enable-annot-syntax)


@export
(defun const-kw-int (kw alist)
  (cadr (assoc kw alist)))

@export
(defun const-int-kw (int alist)
  (car (rassoc int alist)))

