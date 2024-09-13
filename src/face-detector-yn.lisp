(defpackage opencv-jit/face-detector-yn
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package :opencv-jit/face-detector-yn)

(cl-annot:enable-annot-syntax)

@export
(defclass face-detector-yn (cvo) ()
  (:default-initargs
   :free-func #'%face-detector-yn-delete))

@export
(defun make-face-detector-yn (model config size &key (score-threshold 0.9)
                                                  (nms-threshold 0.3) (top-k 5000)
                                                  (backend-id 0) (target-id 0))
  (make-instance 'face-detector-yn
                 :ptr (%face-detector-yn-create model config (cvo-ptr size)
                                                (coerce score-threshold 'float)
                                                (coerce nms-threshold 'float)
                                                top-k backend-id target-id)))
@export
(defmethod face-detector-yn-get-nms-threshold ((fyn face-detector-yn))
  (%face-detector-yn-get-nms-threshold (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-get-score-threshold ((fyn face-detector-yn))
  (%face-detector-yn-get-score-threshold (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-get-top-k ((fyn face-detector-yn))
  (%face-detector-yn-get-top-k (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-set-input-size ((fyn face-detector-yn) size)
  (%face-detector-yn-set-input-size (cvo-ptr fyn) (cvo-ptr size)))


@export
(defmethod face-detector-yn-detect ((fyn face-detector-yn) image)
  (make-instance 'mat
                 :ptr (%face-detector-yn-detect (cvo-ptr fyn) (cvo-ptr image))))
