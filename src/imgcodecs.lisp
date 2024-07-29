(defpackage opencv-jit/imgcodecs
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/util
        #:opencv-jit/foreign
        #:opencv-jit/core)
  (:import-from #:trivial-types
                #:property-list-p))
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

;; ImwriteEXRCompressionFlags
(defconstant-exp +IMWRITE-EXR-COMPRESSION-NO+ 0)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-RLE+ 1)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-ZIPS+ 2)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-ZIP+ 3)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-PIZ+ 4)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-PXR24+ 5)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-B44+ 6)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-B44A+ 7)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-DWAA+ 8)
(defconstant-exp +IMWRITE-EXR-COMPRESSION-DWAB+ 9)

;; ImwriteEXRTypeFlags
(defconstant-exp +IMWRITE-EXR-TYPE-HALF+ 1)
(defconstant-exp +IMWRITE-EXR-TYPE-FLOAT+ 2)

;; ImwriteHDRCompressionFlags
(defconstant-exp +IMWRITE-HDR-COMPRESSION-NONE+ 0)
(defconstant-exp +IMWRITE-HDR-COMPRESSION-RLE+ 1)

;; ImwriteJPEGSamplingFactorParams
(defconstant-exp +IMWRITE-JPEG-SAMPLING-FACTOR-411+ #x411111)
(defconstant-exp +IMWRITE-JPEG-SAMPLING-FACTOR-420+ #x221111)
(defconstant-exp +IMWRITE-JPEG-SAMPLING-FACTOR-422+ #x211111)
(defconstant-exp +IMWRITE-JPEG-SAMPLING-FACTOR-440+ #x121111)
(defconstant-exp +IMWRITE-JPEG-SAMPLING-FACTOR-444+ #x111111)


;; ImwritePAMFlags
(defconstant-exp +IMWRITE-PAM-FORMAT-NULL+ 0)
(defconstant-exp +IMWRITE-PAM-FORMAT-BLACKANDWHITE+ 1)
(defconstant-exp +IMWRITE-PAM-FORMAT-GRAYSCALE+ 2)
(defconstant-exp +IMWRITE-PAM-FORMAT-GRAYSCALE-ALPHA+ 3)
(defconstant-exp +IMWRITE-PAM-FORMAT-RGB+ 4)
(defconstant-exp +IMWRITE-PAM-FORMAT-RGB-ALPHA+ 5)


;; ImwritePNGFlags
(defconstant-exp +IMWRITE-PNG-STRATEGY-DEFAULT+ 0)
(defconstant-exp +IMWRITE-PNG-STRATEGY-FILTERED+ 1)
(defconstant-exp +IMWRITE-PNG-STRATEGY-HUFFMAN-ONLY+ 2)
(defconstant-exp +IMWRITE-PNG-STRATEGY-RLE+ 3)
(defconstant-exp +IMWRITE-PNG-STRATEGY-FIXED+ 4)

;; ImwriteTiffCompressionFlags
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-NONE+ 1)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITTRLE+ 2)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITTFAX3+ 3)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITT-T4+ 3)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITTFAX4+ 4)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITT-T6+ 4)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-LZW+ 5)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-OJPEG+ 6)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-JPEG+ 7)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-T85+ 9)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-T43+ 10)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-NEXT+ 32766)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-CCITTRLEW+ 32771)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-PACKBITS+ 32773)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-THUNDERSCAN+ 32809)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-IT8CTPAD+ 32895)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-IT8LW+ 32896)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-IT8MP+ 32897)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-IT8BL+ 32898)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-PIXARFILM+ 32908)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-PIXARLOG+ 32909)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-DEFLATE+ 32946)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-ADOBE-DEFLATE+ 8)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-DCS+ 32947)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-JBIG+ 34661)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-SGILOG+ 34676)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-SGILOG24+ 34677)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-JP2000+ 34712)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-LERC+ 34887)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-LZMA+ 34925)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-ZSTD+ 50000)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-WEBP+ 50001)
(defconstant-exp +IMWRITE-TIFF-COMPRESSION-JXL+ 50002)

;; ImwriteTiffPredictorFlags
(defconstant-exp +IMWRITE-TIFF-PREDICTOR-NONE+ 1)
(defconstant-exp +IMWRITE-TIFF-PREDICTOR-HORIZONTAL+ 2)
(defconstant-exp +IMWRITE-TIFF-PREDICTOR-FLOATINGPOINT+ 3)

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
(defun imdecode (data  &optional (flag :IMREAD-COLOR))
  (check-type data (vector (unsigned-byte 8)))
  (cffi:with-foreign-array (pointer data (list :array :uchar (length data)))
    (make-instance 'mat
                   :ptr (%imdecode pointer
                                   (length data)
                                   (const-kw-int flag *imread-modes*)))))
@export
(defun imread (path &optional (flag :IMREAD-COLOR))
  (make-instance 'mat
                 :ptr (%imread (etypecase path
                                 (pathname (namestring path))
                                 (string path))
                               (const-kw-int flag *imread-modes*))))

@export
(defun imwrite (path img &rest params)
  (assert (property-list-p params))
  (let ((prep-params (mapcar (lambda (el)
                               (if (keywordp el)
                                   (const-kw-int el *imwrite-flags*)
                                   el))
                             params)))
    (cffi:with-foreign-array (flags-ptr
                              (coerce prep-params 'vector)
                              (list :array :int (length params)))
      (%imwrite (etypecase path
                  (pathname (namestring path))
                  (string path))
                (cvo-ptr img) flags-ptr (length params)))))
