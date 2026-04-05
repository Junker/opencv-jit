(defpackage opencv-jit/highgui
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core)
  (:documentation "High-level GUI functions: windows, display, user input."))
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
  "Create a named window.

Arguments:
  NAME - Window name/title
  FLAGS - Optional window flags (default :AUTOSIZE)
    Possible values: :NORMAL, :AUTOSIZE, :OPENGL, :FULLSCREEN, :FREERATIO, :KEEPRATIO
:GUI-EXPANDED, :GUI-NORMAL"
  (%named-window name (apply #'+ (mapcar (lambda (flag)
                                           (const-kw-int flag *window-flags*))
                                         (or flags '(:AUTOSIZE))))))

@export
(defun destroy-window (name)
  "Destroy the specified window.

Arguments:
  NAME - Window name to destroy"
  (%destroy-window name))

@export
(defun move-window (name x y)
  "Move window to specified position.

Arguments:
  NAME - Window name
  X - X coordinate
  Y - Y coordinate"
  (%move-window name x y))

@export
(defun resize-window (name width height)
  "Resize window to specified dimensions.

Arguments:
  NAME - Window name
  WIDTH - New width in pixels
  HEIGHT - New height in pixels"
  (%resize-window name width height))

@export
(defun set-window-title (name title)
  "Set the window title.

Arguments:
  NAME - Window name
  TITLE - New title string"
  (%set-window-title name title))

@export
(defun imshow (winname img)
  "Display an image in the specified window.

Arguments:
  WINNAME - Window name
  IMG - MAT object to display"
  (%imshow winname (cvo-ptr img)))
