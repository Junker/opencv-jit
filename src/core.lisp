(defpackage opencv-jit/core
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util))
(in-package :opencv-jit/core)

(cl-annot:enable-annot-syntax)

(defconstant +CV-8U+ 0)
(defconstant +CV-8S+ 1)
(defconstant +CV-16U+ 2)
(defconstant +CV-16S+ 3)
(defconstant +CV-32S+ 4)
(defconstant +CV-32F+ 5)
(defconstant +CV-64F+ 6)
(defconstant +CV-16F+ 7)
(defconstant +CV-8UC1+ 0)
(defconstant +CV-8UC2+ 8)
(defconstant +CV-8UC3+ 16)
(defconstant +CV-8UC4+ 24)
(defconstant +CV-8SC1+ 1)
(defconstant +CV-8SC2+ 9)
(defconstant +CV-8SC3+ 17)
(defconstant +CV-8SC4+ 25)
(defconstant +CV-16UC1+ 2)
(defconstant +CV-16UC2+ 10)
(defconstant +CV-16UC3+ 18)
(defconstant +CV-16UC4+ 26)
(defconstant +CV-16SC1+ 3)
(defconstant +CV-16SC2+ 11)
(defconstant +CV-16SC3+ 19)
(defconstant +CV-16SC4+ 27)
(defconstant +CV-32SC1+ 4)
(defconstant +CV-32SC2+ 12)
(defconstant +CV-32SC3+ 20)
(defconstant +CV-32SC4+ 28)
(defconstant +CV-32FC1+ 5)
(defconstant +CV-32FC2+ 13)
(defconstant +CV-32FC3+ 21)
(defconstant +CV-32FC4+ 29)
(defconstant +CV-64FC1+ 6)
(defconstant +CV-64FC2+ 14)
(defconstant +CV-64FC3+ 22)
(defconstant +CV-64FC4+ 30)
(defconstant +CV-16FC1+ 7)
(defconstant +CV-16FC2+ 15)
(defconstant +CV-16FC3+ 23)
(defconstant +CV-16FC4+ 31)

(defvar *mat-depths* `((:CV-8S . ,+CV-8S+)
                       (:CV-8U . +CV-8U+)
                       (:CV-16F . ,+CV-16F+)
                       (:CV-16S . ,+CV-16S+)
                       (:CV-16U . ,+CV-16U+)
                       (:CV-32F . ,+CV-32F+)
                       (:CV-32S . ,+CV-32S+)
                       (:CV-64F . ,+CV-64F+)))

(defvar *mat-types* `((:CV-8UC1 . ,+CV-8UC1+)
                      (:CV-8UC2 . ,+CV-8UC2+)
                      (:CV-8UC3 . ,+CV-8UC3+)
                      (:CV-8UC4 . ,+CV-8UC4+)
                      (:CV-8SC1 . ,+CV-8SC1+)
                      (:CV-8SC2 . ,+CV-8SC2+)
                      (:CV-8SC3 . ,+CV-8SC3+)
                      (:CV-8SC4 . ,+CV-8SC4+)
                      (:CV-16UC1 . ,+CV-16UC1+)
                      (:CV-16UC2 . ,+CV-16UC2+)
                      (:CV-16UC3 . ,+CV-16UC3+)
                      (:CV-16UC4 . ,+CV-16UC4+)
                      (:CV-16SC1 . ,+CV-16SC1+)
                      (:CV-16SC2 . ,+CV-16SC2+)
                      (:CV-16SC3 . ,+CV-16SC3+)
                      (:CV-16SC4 . ,+CV-16SC4+)
                      (:CV-32SC1 . ,+CV-32SC1+)
                      (:CV-32SC2 . ,+CV-32SC2+)
                      (:CV-32SC3 . ,+CV-32SC3+)
                      (:CV-32SC4 . ,+CV-32SC4+)
                      (:CV-32FC1 . ,+CV-32FC1+)
                      (:CV-32FC2 . ,+CV-32FC2+)
                      (:CV-32FC3 . ,+CV-32FC3+)
                      (:CV-32FC4 . ,+CV-32FC4+)
                      (:CV-64FC1 . ,+CV-64FC1+)
                      (:CV-64FC2 . ,+CV-64FC2+)
                      (:CV-64FC3 . ,+CV-64FC3+)
                      (:CV-64FC4 . ,+CV-64FC4+)
                      (:CV-16FC1 . ,+CV-16FC1+)
                      (:CV-16FC2 . ,+CV-16FC2+)
                      (:CV-16FC3 . ,+CV-16FC3+)
                      (:CV-16FC4 . ,+CV-16FC4+)))


(define-condition opencv-error (error)
  ((message :initarg :message
            :reader opencv-error-message)))

@export-class
(defclass cvo ()
  ((ptr :initarg :ptr
        :accessor cvo-ptr))
  (:default-initargs
   :ptr (error "PTR required.")))

;; == Mat
@export
(defclass mat (cvo) ())

;; (defun make-mat (ptr)
;;   (make-instance 'mat :ptr ptr))

(defmethod initialize-instance :after ((mat mat) &key)
  (let ((ptr (cvo-ptr mat)))
    (trivial-garbage:finalize mat
                              (lambda () (%mat-release ptr)))))

@export
(defun make-mat ()
  (make-instance 'mat
                 :ptr (%new-mat)))

@export
(defun make-mat-with-size (rows cols type &optional s)
  (let ((mat (if s
                 (%new-mat-rows-cols-type-s rows cols (const-kw-int type *mat-types*) (cvo-ptr s))
                 (%new-mat-rows-cols-type rows cols (const-kw-int type *mat-types*)))))
    (make-instance 'mat
                   :ptr mat)))

@export
(defmethod mat-at ((mat mat) i0 &optional (i1 nil i1p) (i2 nil i2p))
  (let* ((ptr (cvo-ptr mat))
         (depth (%mat-depth ptr)))
    (cond (i2p
           (ecase depth
             (+CV-8U+ (%mat-at-3d-uchar ptr i0 i1 i2))
             (+CV-8S+ (%mat-at-3d-schar ptr i0 i1 i2))
             (+CV-16U+ (%mat-at-3d-ushort ptr i0 i1 i2))
             (+CV-16S+ (%mat-at-3d-short ptr i0 i1 i2))
             (+CV-32S+ (%mat-at-3d-int ptr i0 i1 i2))
             (+CV-32F+ (%mat-at-3d-float ptr i0 i1 i2))
             (+CV-64F+ (%mat-at-3d-double ptr i0 i1 i2))))
          (i1p
           (ecase depth
             (+CV-8U+ (%mat-at-2d-uchar ptr i0 i1))
             (+CV-8S+ (%mat-at-2d-schar ptr i0 i1))
             (+CV-16U+ (%mat-at-2d-ushort ptr i0 i1))
             (+CV-16S+ (%mat-at-2d-short ptr i0 i1))
             (+CV-32S+ (%mat-at-2d-int ptr i0 i1))
             (+CV-32F+ (%mat-at-2d-float ptr i0 i1))
             (+CV-64F+ (%mat-at-2d-double ptr i0 i1))))
          (t
           (ecase depth
             (+CV-8U+ (%mat-at-1d-uchar ptr i0))
             (+CV-8S+ (%mat-at-1d-schar ptr i0))
             (+CV-16U+ (%mat-at-1d-ushort ptr i0))
             (+CV-16S+ (%mat-at-1d-short ptr i0))
             (+CV-32S+ (%mat-at-1d-int ptr i0))
             (+CV-32F+ (%mat-at-1d-float ptr i0))
             (+CV-64F+ (%mat-at-1d-double ptr i0)))))))

@export
(defmethod mat-depth ((mat mat))
  (const-int-kw (%mat-depth (cvo-ptr mat))
                *mat-depths*))

@export
(defmethod mat-empty-p ((mat mat))
  (%mat-empty (cvo-ptr mat)))

@export
(defmethod mat-total ((mat mat))
  (%mat-total (cvo-ptr mat)))

(defmethod mat-type ((mat mat))
  (const-int-kw (%mat-type (cvo-ptr mat))
                *mat-types*))

@export
(defmethod mat-size ((mat mat))
  (make-instance 'size
                 :ptr (%mat-size (cvo-ptr mat))))

@export
(defmethod mat-type ((mat mat))
  (%mat-type (cvo-ptr mat)))

;; == Size
@export
(defclass size (cvo) ())

(defmethod initialize-instance :after ((size size) &key)
  (let ((ptr (cvo-ptr size)))
    (trivial-garbage:finalize size
                              (lambda () (%size-delete ptr)))))

@export
(defun make-size (&optional (width 0) (height 0))
  (make-instance 'size
                 :ptr (%new-size-wh width height)))

@export
(defmethod size-width ((size size))
  (%size-width (cvo-ptr size)))

@export
(defmethod size-height ((size size))
  (%size-height (cvo-ptr size)))

;; == Scalar
@export
(defclass scalar (cvo) ())

(defmethod initialize-instance :after ((scalar scalar) &key)
  (let ((ptr (cvo-ptr scalar)))
    (trivial-garbage:finalize scalar
                              (lambda () (%scalar-delete ptr)))))

@export
(defun make-scalar (&optional (v0 0) (v1 0) (v2 0) (v3 0))
  (make-instance 'scalar :ptr (%new-scalar4 (coerce v0 'double-float)
                                            (coerce v1 'double-float)
                                            (coerce v2 'double-float)
                                            (coerce v3 'double-float))))

@export
(defmethod scalar-val ((scr scalar) idx)
  (assert (<= idx 4))
  (%scalar-val (cvo-ptr scr) idx))

;; == Point
@export
(defclass point (cvo) ())

(defmethod initialize-instance :after ((pt point) &key)
  (let ((ptr (cvo-ptr pt)))
    (trivial-garbage:finalize pt
                              (lambda () (%point-delete ptr)))))

@export
(defun make-point (&optional (x 0) (y 0))
  (make-instance 'point :ptr (%new-point-xy x y)))

@export
(defmethod point-x ((p point))
  (%point-x (cvo-ptr p)))

@export
(defmethod point-y ((p point))
  (%point-y (cvo-ptr p)))


;; == Rect
@export
(defclass rect (cvo) ())

(defmethod initialize-instance :after ((rect rect) &key)
  (let ((ptr (cvo-ptr rect)))
    (trivial-garbage:finalize rect
                              (lambda () (%rect-delete ptr)))))

@export
(defun make-rect (&optional (x 0) (y 0) (width 0) (height 0))
  (make-instance 'rect :ptr (%new-rect-xywh x y width height)))

@export
(defmethod rect-x ((r rect))
  (%rect-x (cvo-ptr r)))

@export
(defmethod rect-y ((r rect))
  (%rect-y (cvo-ptr r)))

@export
(defmethod rect-width ((r rect))
  (%rect-width (cvo-ptr r)))

@export
(defmethod rect-height ((r rect))
  (%rect-height (cvo-ptr r)))
