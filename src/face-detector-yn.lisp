(defpackage opencv-jit/face-detector-yn
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core)
  (:documentation "Face detection using YuNet model."))
(in-package :opencv-jit/face-detector-yn)

(cl-annot:enable-annot-syntax)

@export
(defclass face-detector-yn (cvo) ()
  (:default-initargs
   :free-func #'%face-detector-yn-delete)
  (:documentation "Face detector using YuNet model.

This class implements face detection using the YuNet deep learning model.
It provides configurable score threshold, NMS threshold, and top-k parameters."))

@export
(defun make-face-detector-yn (model config size &key (score-threshold 0.9)
                                                  (nms-threshold 0.3) (top-k 5000)
                                                  (backend-id 0) (target-id 0))
  "Create a FaceDetectorYN instance.

Arguments:
  MODEL - Path to the face detection model file (ONNX format)
  CONFIG - Path to the model configuration file (can be empty string)
  SIZE - Input SIZE for the detector (width, height)
  :SCORE-THRESHOLD - Minimum confidence for detection (default 0.9)
  :NMS-THRESHOLD - Non-maximum suppression threshold (default 0.3)
  :TOP-K - Maximum number of faces to detect (default 5000)
  :BACKEND-ID - DNN backend to use (default 0 for default backend)
  :TARGET-ID - Target device (default 0 for CPU)"
  (make-instance 'face-detector-yn
                 :ptr (%face-detector-yn-create model config (cvo-ptr size)
                                                (coerce score-threshold 'float)
                                                (coerce nms-threshold 'float)
                                                top-k backend-id target-id)))
@export
(defmethod face-detector-yn-get-nms-threshold ((fyn face-detector-yn))
  "Get the NMS (Non-Maximum Suppression) threshold."
  (%face-detector-yn-get-nms-threshold (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-get-score-threshold ((fyn face-detector-yn))
  "Get the score threshold for face detection."
  (%face-detector-yn-get-score-threshold (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-get-top-k ((fyn face-detector-yn))
  "Get the top-k parameter (maximum faces to detect)."
  (%face-detector-yn-get-top-k (cvo-ptr fyn)))

@export
(defmethod face-detector-yn-set-input-size ((fyn face-detector-yn) size)
  "Set the input size for face detection.

Arguments:
  FYN - A FACE-DETECTOR-YN instance
  SIZE - New input SIZE"
  (%face-detector-yn-set-input-size (cvo-ptr fyn) (cvo-ptr size)))


@export
(defmethod face-detector-yn-detect ((fyn face-detector-yn) image)
  "Detect faces in an image.

Arguments:
  FYN - A FACE-DETECTOR-YN instance
  IMAGE - Input MAT (BGR image)

Returns:
  A MAT containing detected faces."
  (make-instance 'mat
                 :ptr (%face-detector-yn-detect (cvo-ptr fyn) (cvo-ptr image))))
