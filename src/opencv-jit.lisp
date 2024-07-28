(uiop:define-package opencv-jit
    (:use #:cl
          #:cl-annot
          #:cl-annot.class
          #:opencv-jit/foreign
          #:opencv-jit/util
          #:opencv-jit/core
          #:opencv-jit/imgcodecs
          #:opencv-jit/imgproc
          #:opencv-jit/highgui)
  (:reexport #:opencv-jit/core
             #:opencv-jit/imgcodecs
             #:opencv-jit/imgproc
             #:opencv-jit/highgui))
(in-package #:opencv-jit)

;; (cl-annot:enable-annot-syntax)

