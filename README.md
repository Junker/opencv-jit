# OpenCV-JIT

OpenCV 4.x bindings for common Lisp.

OpenCV-JIT uses [CL-CXX-JIT](https://github.com/Islam0mar/CL-CXX-JIT) system
which compiles C++ bindings on system load.

## Requirements

- Installed [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)
- Installed GCC (g++)
- Installed OpenCV 4.5+ (with development files)

## Usage

```common-lisp
(defpackage ocv
  (:use #:cl
        #:opencv-jit
        #:opencv-jit/dnn
        #:opencv-jit/face-detector-yn))
(in-package :ocv)

(defvar *model-text* "/tmp/deploy.prototxt")
(defvar *model-bin* "/tmp/res10_300x300_ssd_iter_140000.caffemodel")
(defvar *net* (read-net-from-caffe *model-text* *model-bin*))

(defun load-from-octets (data)
  (check-type data (vector (unsigned-byte 8)))
  (imdecode data :COLOR))

(defun load-from-file (path)
  (check-type path (or pathname string))
  (if (not (uiop:file-exists-p path))
      (error "file ~S doesn't exists" path)
      (imread path :COLOR)))

(defun get-face-confidences (cvimg)
  (let* ((net-mean (make-scalar 104 177 123))
         (net-size (make-size 300 300))
         (blob (blob-from-image (resize cvimg net-size)
                                :size net-size
                                :mean net-mean)))
    (net-set-input *net* blob)
    (let1 prob (net-forward *net* "")
      (loop :for i :from 0 :to (mat-axis-length prob 2)
            :for confidence := (mat-at prob 0 0 i 2)
            :when (> confidence 0.7)
              :collect confidence))))
```

## Warning

This software is in active development. The APIs will be likely to change.
