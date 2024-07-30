(defpackage opencv-jit/highgui
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package :opencv-jit/highgui)

(cl-annot:enable-annot-syntax)

;; WindowFlags
(defconstant +WINDOW-NORMAL+ #x00000000)
(defconstant +WINDOW-AUTOSIZE+ #x00000001)
(defconstant +WINDOW-OPENGL+ #x00001000)
(defconstant +WINDOW-FULLSCREEN+ 1)
(defconstant +WINDOW-FREERATIO+ #x00000100)
(defconstant +WINDOW-KEEPRATIO+ #x00000000)
(defconstant +WINDOW-GUI-EXPANDED+ #x00000000)
(defconstant +WINDOW-GUI-NORMAL+ #x00000010)

@export
(defvar *window-flags*
  `((:NORMAL . ,+WINDOW-NORMAL+)
    (:AUTOSIZE . ,+WINDOW-AUTOSIZE+)
    (:OPENGL . ,+WINDOW-OPENGL+)
    (:FULLSCREEN . ,+WINDOW-FULLSCREEN+)
    (:FREERATIO . ,+WINDOW-FREERATIO+)
    (:KEEPRATIO . ,+WINDOW-KEEPRATIO+)
    (:GUI-EXPANDED . ,+WINDOW-GUI-EXPANDED+)
    (:GUI-NORMAL . ,+WINDOW-GUI-NORMAL+)))

@export
(defun named-window (name &rest flags)
  (%named-window name (apply #'+ (mapcar (lambda (flag)
                                           (const-kw-int flag *window-flags*))
                                         (or flags '(:AUTOSIZE))))))

@export
(defun destroy-window (name)
  (%destroy-window name))

@export
(defun move-window (name x y)
  (%move-window name x y))

@export
(defun resize-window (name width height)
  (%resize-window name width height))

@export
(defun set-window-title (name title)
  (%set-window-title name title))

@export
(defun imshow (winname img)
  (%imshow winname (cvo-ptr img)))
