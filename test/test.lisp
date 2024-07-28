(defpackage #:opencv-jit-test
  (:use #:cl #:parachute #:opencv-jit))

(in-package #:opencv-jit-test)

(define-test size-create
    (let ((s (opencv-jit/core:make-size 10 20)))
      (is = (opencv-jit/core:size-width s) 10)
      (is = (opencv-jit/core:size-height s) 20)))

(define-test scalar-create
    (let ((s (opencv-jit/core:make-scalar 10 20 30 40)))
      (is = (opencv-jit/core:scalar-val s 0) 10)
      (is = (opencv-jit/core:scalar-val s 3) 40)))

(define-test rect-create
    (let ((r (opencv-jit/core:make-rect 10 20 50 100)))
      (is = (opencv-jit/core:rect-width r) 50)
      (is = (opencv-jit/core:rect-y r) 20)))

(define-test point-create
    (let ((p (opencv-jit/core:make-point 10 20)))
      (is = (opencv-jit/core:point-x p) 10)
      (is = (opencv-jit/core:point-y p) 20)))
