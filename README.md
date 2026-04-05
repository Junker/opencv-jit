# OpenCV-JIT

OpenCV 4.x bindings for Common Lisp.

OpenCV-JIT uses [CL-CXX-JIT](https://github.com/Islam0mar/CL-CXX-JIT) system
which compiles C++ bindings on system load, providing JIT compilation of C++
code at load time instead of pre-compiled FFI bindings.

## Requirements

- Installed [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)
- Installed GCC (g++)
- Installed OpenCV 4.5+ (with development files)

## Installation

Clone the repository and load with Quicklisp:

```common-lisp
(ql:quickload :opencv-jit)
```

## API Overview

### Core Classes

#### MAT - Matrix/Image representation

```common-lisp
;; Create empty matrix
(make-mat)

;; Create matrix with size and type
(make-mat-with-size 100 100 :8UC3)  ; 100x100 3-channel BGR

;; Create from file
(imread #p"/path/to/image.png")
(imread #p"/path/to/image.png" :GRAYSCALE)

;; Decode from octets
(imdecode #(137 80 78 71 ...) :COLOR)

;; Save to file
(imwrite #p"/tmp/output.jpg" mat :JPEG-QUALITY 95)
```

#### SIZE - Size/Dimensions

```common-lisp
(make-size 640 480)
(size-width s)  ; => 640
(size-height s) ; => 480
```

#### SCALAR - Multi-value (up to 4 channels)

```common-lisp
(make-scalar 255 0 0)  ; Blue for BGR image
(make-scalar 104 177 123)  ; Mean values for DNN
(scalar-val s 0)  ; Get first value
```

#### POINT - 2D Point

```common-lisp
(make-point 10 20)
(point-x p)  ; => 10
(point-y p)  ; => 20
```

#### RECT - Rectangle

```common-lisp
(make-rect 10 20 100 50)  ; x y width height
(rect-x r)      ; => 10
(rect-y r)      ; => 20
(rect-width r)  ; => 100
(rect-height r) ; => 50
```

### Image Processing

```common-lisp
;; Color conversion
(cvt-color img :BGR2GRAY)     ; BGR to grayscale
(cvt-color img :BGR2HSV)      ; BGR to HSV

;; Resize
(resize img (make-size 300 300))
(resize img (make-size 100 100) :interpolation :CUBIC)
```

### HighGUI - Window Management

```common-lisp
;; Create and show
(named-window "Preview")
(imshow "Preview" mat)
(waitkey 0)  ; Wait for key press

;; Window management
(destroy-window "Preview")
(move-window "Preview" 100 100)
(resize-window "Preview" 640 480)
(set-window-title "Preview" "My Image")
```

### DNN - Deep Neural Networks

```common-lisp
;; Load network from different formats
(read-net-from-caffe "deploy.prototxt" "model.caffemodel")
(read-net-from-onnx "model.onnx")
(read-net-from-tensorflow "model.pb" "")
(read-net-from-darknet "model.cfg" "model.weights")

;; Prepare input blob
(let* ((net (read-net-from-caffe "deploy.prototxt" "model.caffemodel"))
       (img (imread "image.jpg"))
       (blob (blob-from-image img
                              :size (make-size 300 300)
                              :mean (make-scalar 104 177 123)
                              :scale-factor 1.0)))
  ;; Run inference
  (net-set-input net blob)
  (let ((output (net-forward net)))
    ;; Process output
    ))
```

### Face Detection (YuNet)

```common-lisp
;; Create face detector
(let* ((detector (make-face-detector-yn
                   "face_detection_yunet.onnx"
                   ""
                   (make-size 640 480)
                   :score-threshold 0.9))
       (img (imread "photo.jpg"))
       (faces (face-detector-yn-detect detector img)))
  ;; faces is a MAT where each row is one face:
  ;; [x, y, w, h, x_re, y_re, x_le, y_le, x_nt, y_nt, x_rcm, y_rcm, x_lcm, y_lcm, score]
  )
```

### MAT Access Methods

```common-lisp
;; Get element at position
(mat-at mat 10 20)  ; For 2D single-channel, returns value
(mat-at mat 10 20)  ; For 2D multi-channel, returns VEC

;; VEC operations for multi-channel
(let ((v (mat-at mat 10 20)))
  (vec-val v 0)        ; First channel (Blue for BGR)
  (vec-val v 1)        ; Second channel (Green for BGR)
  (vec-to-list v)      ; => (B G R)
  (vec-to-vector v))   ; => #(B G R)

;; Matrix properties
(mat-rows mat)         ; Number of rows
(mat-cols mat)         ; Number of columns
(mat-channels mat)     ; Number of channels
(mat-depth mat)        ; Depth type (:8U, :32F, etc.)
(mat-type mat)         ; Full type (:8UC3, :32FC1, etc.)
(mat-dims mat)         ; Number of dimensions
(mat-total mat)        ; Total elements
(mat-size mat)         ; SIZE object
(mat-empty-p mat)      ; Check if empty

;; Convert to Lisp array (single-channel only)
(mat-to-array mat)
(mat-data mat)
```

## Example: Face Detection with DNN

```common-lisp
(defpackage ocv
  (:use #:cl
        #:opencv-jit
        #:opencv-jit/dnn))
(in-package :ocv)

(defvar *model-text* "/path/to/deploy.prototxt")
(defvar *model-bin* "/path/to/res10_300x300_ssd_iter_140000.caffemodel")
(defvar *net* (read-net-from-caffe *model-text* *model-bin*))

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
    (let ((prob (net-forward *net* "")))
      (loop :for i :from 0 :below (mat-axis-length prob 2)
            :for confidence := (mat-at prob 0 0 i 2)
            :when (> confidence 0.7)
              :collect confidence))))
```

## Available Color Conversion Codes

Common codes for `cvt-color`:

- `:BGR2GRAY` - BGR to grayscale
- `:GRAY2BGR` - Grayscale to BGR
- `:BGR2HSV` - BGR to HSV
- `:HSV2BGR` - HSV to BGR
- `:BGR2RGB` - Swap B and R channels
- `:RGB2BGR` - Swap R and B channels

## Available Interpolation Methods

For `resize`:

- `:NEAREST` - Nearest neighbor interpolation
- `:LINEAR` - Bilinear interpolation (default)
- `:CUBIC` - Bicubic interpolation
- `:AREA` - Resampling using pixel area relation
- `:LANCZOS4` - Lanczos interpolation over 8x8 neighborhood

## MAT Types

Matrix element types:

- `:8UC1`, `:8UC2`, `:8UC3`, `:8UC4` - 8-bit unsigned (uchar)
- `:8SC1`, `:8SC2`, `:8SC3`, `:8SC4` - 8-bit signed (schar)
- `:16UC1`, `:16UC2`, `:16UC3`, `:16UC4` - 16-bit unsigned (ushort)
- `:16SC1`, `:16SC2`, `:16SC3`, `:16SC4` - 16-bit signed (short)
- `:32SC1`, `:32SC2`, `:32SC3`, `:32SC4` - 32-bit signed (int)
- `:32FC1`, `:32FC2`, `:32FC3`, `:32FC4` - 32-bit float
- `:64FC1`, `:64FC2`, `:64FC3`, `:64FC4` - 64-bit double

## Warning

This software is in active development. The APIs may change.
