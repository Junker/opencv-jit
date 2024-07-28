(defpackage opencv-jit/highgui
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package :opencv-jit/highgui)

(cl-annot:enable-annot-syntax)
