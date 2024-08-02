(defpackage opencv-jit/dnn
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package #:opencv-jit/dnn)

(cl-annot:enable-annot-syntax)

;; Backend
@export
(defclass net (cvo) ()
  (:default-initargs
   :free-func #'%dnn-net-delete))

@export
(defun read-net-from-caffe (prototxt model)
  "Reads a network model stored in Caffe framework's format. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-caffe prototxt model)))

@export
(defun read-net-from-darknet (cfg-file model)
  "Reads a network model stored in Darknet model files. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-darknet cfg-file model)))

@export
(defun read-net-from-model-optimizer (xml bin)
  "Load a network from Intel's Model Optimizer intermediate representation."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-darknet xml bin)))

@export
(defun read-net-from-onnx (onnx-file)
  "Reads a network model ONNX. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-onnx onnx-file)))

@export
(defun read-net-from-tensorflow (model config)
  "Reads a network model stored in TensorFlow framework's format. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-tensorflow model config)))

@export
(defun read-net-from-tflite (model)
  "Reads a network model stored in TFLite framework's format. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-tflite model)))

@export
(defun read-net-from-torch (model binaryp evaluatep)
  "Reads a network model stored in Torch7 framework's format. "
  (make-instance 'net
                 :ptr (%dnn-read-net-from-torch model binaryp evaluatep)))

@export
(defun blob-from-image (image &key (scale-factor 1.0) size mean swap-rb crop)
  "Creates 4-dimensional blob from image. Optionally resizes and crops image from center,
  subtract mean values, scales values by scale-factor, swap Blue and Red channels."
  (make-instance 'mat
                 :ptr (%dnn-blob-from-image (cvo-ptr image)
                                            (coerce scale-factor 'double-float)
                                            (cvo-ptr (or size (make-size)))
                                            (cvo-ptr (or mean (make-scalar)))
                                            swap-rb
                                            crop)))


;; == METHODS

@export
(defmethod net-dump ((net net))
  (%dnn-net-dump (cvo-ptr net)))

@export
(defmethod net-empty ((net net))
  (%dnn-net-empty (cvo-ptr net)))

@export
(defmethod net-enable-fusion ((net net) enablep)
  (%dnn-net-enable-fusion (cvo-ptr net) enablep))

@export
(defmethod net-enable-winograd ((net net) enablep)
  (%dnn-net-enable-winograd (cvo-ptr net) enablep))

@export
(defmethod net-set-input ((net net) input &key (name "") (scale-factor 1.0) mean)
  (%dnn-net-set-input (cvo-ptr net)
                      (cvo-ptr input)
                      name
                      (coerce scale-factor 'double-float)
                      (cvo-ptr (or mean (make-scalar)))))

@export
(defmethod net-set-input-shape ((net net) name shape)
  (%dnn-net-set-input-shape (cvo-ptr net) name (cvo-ptr shape)))

@export
(defmethod net-forward ((net net) &optional (output-name ""))
  (make-instance 'mat
                 :ptr (%dnn-net-forward (cvo-ptr net) output-name)))
