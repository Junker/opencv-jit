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

@export
(defvar *mat-depths* `((:8U . ,+CV-8U+)
                       (:8S . ,+CV-8S+)
                       (:16F . ,+CV-16F+)
                       (:16S . ,+CV-16S+)
                       (:16U . ,+CV-16U+)
                       (:32F . ,+CV-32F+)
                       (:32S . ,+CV-32S+)
                       (:64F . ,+CV-64F+)))

@export
(defvar *mat-types* `((:8UC1 . ,+CV-8UC1+)
                      (:8UC2 . ,+CV-8UC2+)
                      (:8UC3 . ,+CV-8UC3+)
                      (:8UC4 . ,+CV-8UC4+)
                      (:8SC1 . ,+CV-8SC1+)
                      (:8SC2 . ,+CV-8SC2+)
                      (:8SC3 . ,+CV-8SC3+)
                      (:8SC4 . ,+CV-8SC4+)
                      (:16UC1 . ,+CV-16UC1+)
                      (:16UC2 . ,+CV-16UC2+)
                      (:16UC3 . ,+CV-16UC3+)
                      (:16UC4 . ,+CV-16UC4+)
                      (:16SC1 . ,+CV-16SC1+)
                      (:16SC2 . ,+CV-16SC2+)
                      (:16SC3 . ,+CV-16SC3+)
                      (:16SC4 . ,+CV-16SC4+)
                      (:32SC1 . ,+CV-32SC1+)
                      (:32SC2 . ,+CV-32SC2+)
                      (:32SC3 . ,+CV-32SC3+)
                      (:32SC4 . ,+CV-32SC4+)
                      (:32FC1 . ,+CV-32FC1+)
                      (:32FC2 . ,+CV-32FC2+)
                      (:32FC3 . ,+CV-32FC3+)
                      (:32FC4 . ,+CV-32FC4+)
                      (:64FC1 . ,+CV-64FC1+)
                      (:64FC2 . ,+CV-64FC2+)
                      (:64FC3 . ,+CV-64FC3+)
                      (:64FC4 . ,+CV-64FC4+)
                      (:16FC1 . ,+CV-16FC1+)
                      (:16FC2 . ,+CV-16FC2+)
                      (:16FC3 . ,+CV-16FC3+)
                      (:16FC4 . ,+CV-16FC4+)))


(define-condition opencv-error (error)
  ((message :initarg :message
            :reader opencv-error-message)))

@export-class
(defclass cvo ()
  ((ptr :initarg :ptr
        :reader cvo-ptr))
  (:default-initargs
   :ptr (error "PTR required.")))

;;  ===================== Vec
(deftype vectype () '(member :uchar :schar :double :float :int :short :ushort))

@export-class
(defclass vec (cvo)
  ((len :initarg :len
        :reader vec-len)
   (type :initarg :type
         :reader vec-type
         :type vectype))
  (:default-initargs
   :len (error "LEN required.")
   :type (error "TYPE required.")))

(defmethod initialize-instance :after ((v vec) &key)
  (let ((ptr (cvo-ptr v))
        (type (vec-type v))
        (len (vec-len v)))
    (trivial-garbage:finalize v
                              (lambda ()
                                (case type
                                  (:uchar (case len
                                            (2 (%vec-uchar2-delete ptr))
                                            (3 (%vec-uchar3-delete ptr))
                                            (4 (%vec-uchar4-delete ptr))))
                                  (:schar (case len
                                            (2 (%vec-schar2-delete ptr))
                                            (3 (%vec-schar3-delete ptr))
                                            (4 (%vec-schar4-delete ptr))))
                                  (:double (case len
                                             (2 (%vec-double2-delete ptr))
                                             (3 (%vec-double3-delete ptr))
                                             (4 (%vec-double4-delete ptr))))
                                  (:float (case len
                                            (2 (%vec-float2-delete ptr))
                                            (3 (%vec-float3-delete ptr))
                                            (4 (%vec-float4-delete ptr))))
                                  (:int (case len
                                          (2 (%vec-int2-delete ptr))
                                          (3 (%vec-int3-delete ptr))
                                          (4 (%vec-int4-delete ptr))))
                                  (:short (case len
                                            (2 (%vec-short2-delete ptr))
                                            (3 (%vec-short3-delete ptr))
                                            (4 (%vec-short4-delete ptr))))
                                  (:ushort (case len
                                             (2 (%vec-ushort2-delete ptr))
                                             (3 (%vec-ushort3-delete ptr))
                                             (4 (%vec-ushort4-delete ptr)))))))))

@export
(defmethod vec-val ((v vec) i)
  (assert (< i (vec-len v)))
  (let ((ptr (cvo-ptr v))
        (type (vec-type v))
        (len (vec-len v)))
    (case type
      (:uchar (case len
                (2 (%vec-uchar2-val ptr i))
                (3 (%vec-uchar3-val ptr i))
                (4 (%vec-uchar4-val ptr i))))
      (:schar (case len
                (2 (%vec-schar2-val ptr i))
                (3 (%vec-schar3-val ptr i))
                (4 (%vec-schar4-val ptr i))))
      (:double (case len
                 (2 (%vec-double2-val ptr i))
                 (3 (%vec-double3-val ptr i))
                 (4 (%vec-double4-val ptr i))))
      (:float (case len
                (2 (%vec-float2-val ptr i))
                (3 (%vec-float3-val ptr i))
                (4 (%vec-float4-val ptr i))))
      (:int (case len
              (2 (%vec-int2-val ptr i))
              (3 (%vec-int3-val ptr i))
              (4 (%vec-int4-val ptr i))))
      (:short (case len
                (2 (%vec-short2-val ptr i))
                (3 (%vec-short3-val ptr i))
                (4 (%vec-short4-val ptr i))))
      (:ushort (case len
                 (2 (%vec-ushort2-val ptr i))
                 (3 (%vec-ushort3-val ptr i))
                 (4 (%vec-ushort4-val ptr i)))))))

@export
(defmethod vec-to-list ((v vec))
  (append (list (vec-val v 0) (vec-val v 1))
          (when (> (vec-len v) 2)
            (list (vec-val v 2)))
          (when (> (vec-len v) 3)
            (list (vec-val v 3)))))
@export
(defmethod vec-to-vector ((v vec))
  (make-array (vec-len v)
              :initial-contents (vec-to-list v)))

;;  ===================== Mat
@export
(defclass mat (cvo) ())

;; (defun make-mat (ptr)
;;   (make-instance 'mat :ptr ptr))

(defmethod initialize-instance :after ((m mat) &key)
  (let ((ptr (cvo-ptr m)))
    (trivial-garbage:finalize m
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
(defmethod mat-at ((m mat) i0 &optional (i1 0 i1p) (i2 0 i2p))
  (let* ((ptr (cvo-ptr m))
         (type (mat-type m))
         (dims (mat-dims m)))
    (assert (case dims
              (1 (not i1p))
              (2 (and i1p (not i2p)))
              (3 i1p i2p)))
    (flet ((%mvec (ptr type len)
             (make-instance 'vec :ptr ptr :type type :len len)))
      (ecase type
        (:8UC1 (%mat-at-3d-uchar ptr i0 i1 i2))
        (:8UC2 (%mvec (%mat-at-3d-uchar2 ptr i0 i1 i2) :uchar 2))
        (:8UC3 (%mvec (%mat-at-3d-uchar3 ptr i0 i1 i2) :uchar 3))
        (:8UC4 (%mvec (%mat-at-3d-uchar4 ptr i0 i1 i2) :uchar 4))
        (:8SC1 (%mat-at-3d-schar ptr i0 i1 i2))
        (:8SC2 (%mvec (%mat-at-3d-schar2 ptr i0 i1 i2) :schar 2))
        (:8SC3 (%mvec (%mat-at-3d-schar3 ptr i0 i1 i2) :schar 3))
        (:8SC4 (%mvec (%mat-at-3d-schar4 ptr i0 i1 i2) :schar 4))
        (:16UC1 (%mat-at-3d-ushort ptr i0 i1 i2))
        (:16UC2 (%mvec (%mat-at-3d-ushort2 ptr i0 i1 i2) :ushort 2))
        (:16UC3 (%mvec (%mat-at-3d-ushort3 ptr i0 i1 i2) :ushort 3))
        (:16UC4 (%mvec (%mat-at-3d-ushort4 ptr i0 i1 i2) :ushort 4))
        (:16SC1 (%mat-at-3d-short ptr i0 i1 i2))
        (:16SC2 (%mvec (%mat-at-3d-short2 ptr i0 i1 i2) :short 2))
        (:16SC3 (%mvec (%mat-at-3d-short3 ptr i0 i1 i2) :short 3))
        (:16SC4 (%mvec (%mat-at-3d-short4 ptr i0 i1 i2) :short 4))
        (:32SC1 (%mat-at-3d-int ptr i0 i1 i2))
        (:32SC2 (%mvec (%mat-at-3d-int2 ptr i0 i1 i2) :int 2))
        (:32SC3 (%mvec (%mat-at-3d-int3 ptr i0 i1 i2) :int 3))
        (:32SC4 (%mvec (%mat-at-3d-int4 ptr i0 i1 i2) :int 4))
        (:32FC1 (%mat-at-3d-float ptr i0 i1 i2))
        (:32FC2 (%mvec (%mat-at-3d-float2 ptr i0 i1 i2) :float 2))
        (:32FC3 (%mvec (%mat-at-3d-float3 ptr i0 i1 i2) :float 3))
        (:32FC4 (%mvec (%mat-at-3d-float4 ptr i0 i1 i2) :float 4))
        (:64FC1 (%mat-at-3d-double ptr i0 i1 i2))
        (:64FC2 (%mvec (%mat-at-3d-double2 ptr i0 i1 i2) :double 2))
        (:64FC3 (%mvec (%mat-at-3d-double3 ptr i0 i1 i2) :double 3))
        (:64FC4 (%mvec (%mat-at-3d-double4 ptr i0 i1 i2) :double 4))))))


@export
(defmethod mat-channels ((m mat))
  (%mat-channels (cvo-ptr m)))

@export
(defmethod mat-col ((m mat) idx)
  (make-instance 'mat
                 :ptr (%mat-col (cvo-ptr m) idx)))

@export
(defmethod mat-cols ((m mat))
  (%mat-cols (cvo-ptr m)))

@export
(defmethod mat-depth ((m mat))
  (const-int-kw (%mat-depth (cvo-ptr m))
                *mat-depths*))

@export
(defmethod mat-dims ((m mat))
  (%mat-dims (cvo-ptr m)))

@export
(defmethod mat-empty-p ((m mat))
  (%mat-empty (cvo-ptr m)))

@export
(defmethod mat-row ((m mat) idx)
  (make-instance 'mat
                 :ptr (%mat-row (cvo-ptr m) idx)))

@export
(defmethod mat-rows ((m mat))
  (%mat-rows (cvo-ptr m)))

@export
(defmethod mat-total ((m mat))
  (%mat-total (cvo-ptr m)))

@export
(defmethod mat-type ((m mat))
  (const-int-kw (%mat-type (cvo-ptr m))
                *mat-types*))

@export
(defmethod mat-size ((m mat))
  (make-instance 'size
                 :ptr (%mat-size (cvo-ptr m))))

;;  ===================== Size
@export
(defclass size (cvo) ())

(defmethod initialize-instance :after ((sz size) &key)
  (let ((ptr (cvo-ptr sz)))
    (trivial-garbage:finalize sz
                              (lambda () (%size-delete ptr)))))

@export
(defun make-size (&optional (width 0) (height 0))
  (make-instance 'size
                 :ptr (%new-size-wh width height)))

@export
(defmethod size-width ((sz size))
  (%size-width (cvo-ptr sz)))

@export
(defmethod size-height ((sz size))
  (%size-height (cvo-ptr sz)))

;;  ===================== Scalar
@export
(defclass scalar (vec) ())

(defmethod initialize-instance :after ((sc scalar) &key)
  (let ((ptr (cvo-ptr sc)))
    (trivial-garbage:finalize sc
                              (lambda () (%scalar-delete ptr)))))

@export
(defun make-scalar (&optional (v0 0) (v1 0) (v2 0) (v3 0))
  (make-instance 'scalar
                 :len 4
                 :type :double
                 :ptr (%new-scalar4 (coerce v0 'double-float)
                                    (coerce v1 'double-float)
                                    (coerce v2 'double-float)
                                    (coerce v3 'double-float))))

@export
(defmethod scalar-val ((sc scalar) idx)
  (vec-val sc idx))

@export
(defmethod scalar-to-list ((sc scalar))
  (vec-to-list sc))

@export
(defmethod scalar-to-vector ((sc scalar))
  (vec-to-vector sc))

;;  ===================== Point
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


;; ===================== Rect

@export
(defclass rect (cvo) ())

(defmethod initialize-instance :after ((r rect) &key)
  (let ((ptr (cvo-ptr r)))
    (trivial-garbage:finalize r
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
