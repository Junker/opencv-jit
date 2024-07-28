(defpackage opencv-jit/imgcodecs
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package :opencv-jit/imgcodecs)

(cl-annot:enable-annot-syntax)

;; ImreadModes
(defconstant +IMREAD-UNCHANGED+ -1)
(defconstant +IMREAD-GRAYSCALE+ 0)
(defconstant +IMREAD-COLOR+ 1)
(defconstant +IMREAD-ANYDEPTH+ 2)
(defconstant +IMREAD-ANYCOLOR+ 4)
(defconstant +IMREAD-LOAD-GDAL+ 8)
(defconstant +IMREAD-REDUCED-GRAYSCALE-2+ 16)
(defconstant +IMREAD-REDUCED-COLOR-2+ 17)
(defconstant +IMREAD-REDUCED-GRAYSCALE-4+ 32)
(defconstant +IMREAD-REDUCED-COLOR-4+ 33)
(defconstant +IMREAD-REDUCED-GRAYSCALE-8+ 64)
(defconstant +IMREAD-REDUCED-COLOR-8+ 65)
(defconstant +IMREAD-IGNORE-ORIENTATION+ 128)

;; ImwriteFlags
(defconstant +IMWRITE-JPEG-QUALITY+ 1)
(defconstant +IMWRITE-JPEG-PROGRESSIVE+ 2)
(defconstant +IMWRITE-JPEG-OPTIMIZE+ 3)
(defconstant +IMWRITE-JPEG-RST-INTERVAL+ 4)
(defconstant +IMWRITE-JPEG-LUMA-QUALITY+ 5)
(defconstant +IMWRITE-JPEG-CHROMA-QUALITY+ 6)
(defconstant +IMWRITE-JPEG-SAMPLING-FACTOR+ 7)
(defconstant +IMWRITE-PNG-COMPRESSION+ 16)
(defconstant +IMWRITE-PNG-STRATEGY+ 17)
(defconstant +IMWRITE-PNG-BILEVEL+ 18)
(defconstant +IMWRITE-PXM-BINARY+ 32)
(defconstant +IMWRITE-EXR-TYPE+ 48)
(defconstant +IMWRITE-WEBP-QUALITY+ 64)
(defconstant +IMWRITE-HDR-COMPRESSION+ 80)
(defconstant +IMWRITE-PAM-TUPLETYPE+ 128)
(defconstant +IMWRITE-TIFF-RESUNIT+ 256)
(defconstant +IMWRITE-TIFF-XDPI+ 257)
(defconstant +IMWRITE-TIFF-YDPI+ 258)
(defconstant +IMWRITE-TIFF-COMPRESSION+ 259)

(defvar *imread-modes*
  `((:IMREAD-UNCHANGED . ,+IMREAD-UNCHANGED+)
    (:IMREAD-GRAYSCALE . ,+IMREAD-GRAYSCALE+)
    (:IMREAD-COLOR . ,+IMREAD-COLOR+)
    (:IMREAD-ANYDEPTH . ,+IMREAD-ANYDEPTH+)
    (:IMREAD-ANYCOLOR . ,+IMREAD-ANYCOLOR+)
    (:IMREAD-LOAD-GDAL . ,+IMREAD-LOAD-GDAL+)
    (:IMREAD-REDUCED-GRAYSCALE-2 . ,+IMREAD-REDUCED-GRAYSCALE-2+)
    (:IMREAD-REDUCED-COLOR-2 . ,+IMREAD-REDUCED-COLOR-2+)
    (:IMREAD-REDUCED-GRAYSCALE-4 . ,+IMREAD-REDUCED-GRAYSCALE-4+)
    (:IMREAD-REDUCED-COLOR-4 . ,+IMREAD-REDUCED-COLOR-4+)
    (:IMREAD-REDUCED-GRAYSCALE-8 . ,+IMREAD-REDUCED-GRAYSCALE-8+)
    (:IMREAD-REDUCED-COLOR-8 . ,+IMREAD-REDUCED-COLOR-8+)
    (:IMREAD-IGNORE-ORIENTATION . ,+IMREAD-IGNORE-ORIENTATION+)))

(defvar *imwrite-flags*
  `((:IMWRITE-JPEG-QUALITY . ,+IMWRITE-JPEG-QUALITY+)
    (:IMWRITE-JPEG-PROGRESSIVE . ,+IMWRITE-JPEG-PROGRESSIVE+)
    (:IMWRITE-JPEG-OPTIMIZE . ,+IMWRITE-JPEG-OPTIMIZE+)
    (:IMWRITE-JPEG-RST-INTERVAL . ,+IMWRITE-JPEG-RST-INTERVAL+)
    (:IMWRITE-JPEG-LUMA-QUALITY . ,+IMWRITE-JPEG-LUMA-QUALITY+)
    (:IMWRITE-JPEG-CHROMA-QUALITY . ,+IMWRITE-JPEG-CHROMA-QUALITY+)
    (:IMWRITE-JPEG-SAMPLING-FACTOR . ,+IMWRITE-JPEG-SAMPLING-FACTOR+)
    (:IMWRITE-PNG-COMPRESSION . ,+IMWRITE-PNG-COMPRESSION+)
    (:IMWRITE-PNG-STRATEGY . ,+IMWRITE-PNG-STRATEGY+)
    (:IMWRITE-PNG-BILEVEL . ,+IMWRITE-PNG-BILEVEL+)
    (:IMWRITE-PXM-BINARY . ,+IMWRITE-PXM-BINARY+)
    (:IMWRITE-EXR-TYPE . ,+IMWRITE-EXR-TYPE+)
    (:IMWRITE-WEBP-QUALITY . ,+IMWRITE-WEBP-QUALITY+)
    (:IMWRITE-HDR-COMPRESSION . ,+IMWRITE-HDR-COMPRESSION+)
    (:IMWRITE-PAM-TUPLETYPE . ,+IMWRITE-PAM-TUPLETYPE+)
    (:IMWRITE-TIFF-RESUNIT . ,+IMWRITE-TIFF-RESUNIT+)
    (:IMWRITE-TIFF-XDPI . ,+IMWRITE-TIFF-XDPI+)
    (:IMWRITE-TIFF-YDPI . ,+IMWRITE-TIFF-YDPI+)
    (:IMWRITE-TIFF-COMPRESSION . ,+IMWRITE-TIFF-COMPRESSION+)))

@export
(defun imread (path &optional (mode :IMREAD-COLOR))
  (make-instance 'mat
                 :ptr (%imread (etypecase path
                                 (pathname (namestring path))
                                 (string path))
                               (const-kw-int mode *imread-modes*))))

@export
(defun imwrite (path img &rest params)
  (declare (ignore params)) ; TODO TEMP
  (%imwrite path (cvo-ptr img)))
