(defpackage opencv-jit/dnn
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core))
(in-package #:opencv-jit/dnn)

(cl-annot:enable-annot-syntax)

@export
(defclass net (cvo) ())

@export
(defmethod initialize-instance :after ((net net) &key)
  (let ((ptr (cvo-ptr net)))
    (trivial-garbage:finalize net
                              (lambda () (%dnn-net-delete ptr)))))

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
                                            scale-factor
                                            (or size (%new-size))
                                            (or mean (%new-scalar))
                                            swap-rb
                                            crop)))


;; == METHODS

@export
(defmethod net-set-input ((n net) input &key (name "") (scale-factor 1.0) mean)
  (%dnn-net-set-input (cvo-ptr n) (cvo-ptr input)
                      name scale-factor (or mean (make-scalar))))

@export
(defmethod net-forward ((n net) name)
  (make-instance 'mat
                 :ptr (%dnn-net-forward n name)))
