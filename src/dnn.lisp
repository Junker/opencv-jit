(defpackage opencv-jit/dnn
  (:use #:cl
        #:cl-annot
        #:cl-annot.class
        #:opencv-jit/foreign
        #:opencv-jit/util
        #:opencv-jit/core)
  (:documentation "Deep Neural Network module for loading and running inference."))
(in-package :opencv-jit/dnn)

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
  "Reads a network model stored in Darknet model files.

Arguments:
  CFG-FILE - Path to the .cfg configuration file
  MODEL - Path to the .weights model file

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-darknet cfg-file model)))

@export
(defun read-net-from-model-optimizer (xml bin)
  "Load a network from Intel's Model Optimizer intermediate representation.

Arguments:
  XML - Path to the .xml model file
  BIN - Path to the .bin weights file

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-model-optimizer xml bin)))

@export
(defun read-net-from-onnx (onnx-file)
  "Reads a network model stored in ONNX format.

Arguments:
  ONNX-FILE - Path to the .onnx model file

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-onnx onnx-file)))

@export
(defun read-net-from-tensorflow (model config)
  "Reads a network model stored in TensorFlow framework's format.

Arguments:
  MODEL - Path to the .pb model file
  CONFIG - Path to the .pbtxt config file (can be empty string)

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-tensorflow model config)))

@export
(defun read-net-from-tflite (model)
  "Reads a network model stored in TFLite framework's format.

Arguments:
  MODEL - Path to the .tflite model file

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-tflite model)))

@export
(defun read-net-from-torch (model binaryp evaluatep)
  "Reads a network model stored in Torch7 framework's format.

Arguments:
  MODEL - Path to the model file
  BINARYP - Whether the model is in binary format
  EVALUATEP - Whether to evaluate the network

Returns:
  A NET object."
  (make-instance 'net
                 :ptr (%dnn-read-net-from-torch model binaryp evaluatep)))

@export
(defun blob-from-image (image &key (scale-factor 1.0) size mean swap-rb crop)
  "Creates 4-dimensional blob from image.

Optionally resizes and crops image from center, subtract mean values,
scales values by scale-factor, swap Blue and Red channels.

Arguments:
  IMAGE - Source MAT (should be 3-channel BGR image)
  :SCALE-FACTOR - Multiplier for image values (default 1.0)
  :SIZE - Target SIZE for spatial dimensions (default: original size)
  :MEAN - SCALAR with mean values to subtract per channel
  :SWAP-RB - Whether to swap Red and Blue channels (default NIL)
  :CROP - Whether to crop image after resize (default NIL)

Returns:
  A MAT representing a 4D blob (NCHW format).

Example:
  (blob-from-image img :size (make-size 300 300) :mean (make-scalar 104 177 123))"
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
  "Dump NET to string representation.

Arguments:
  NET - A NET object

Returns:
  String representation of the network."
  (%dnn-net-dump (cvo-ptr net)))

@export
(defmethod net-empty ((net net))
  "Check if NET is empty."
  (%dnn-net-empty (cvo-ptr net)))

@export
(defmethod net-enable-fusion ((net net) enablep)
  "Enable or disable network fusion optimization.

Arguments:
  NET - A NET object
  ENABLEP - T to enable, NIL to disable"
  (%dnn-net-enable-fusion (cvo-ptr net) enablep))

@export
(defmethod net-enable-winograd ((net net) enablep)
  "Enable or disable Winograd convolution optimization.

Arguments:
  NET - A NET object
  ENABLEP - T to enable, NIL to disable"
  (%dnn-net-enable-winograd (cvo-ptr net) enablep))

@export
(defmethod net-set-input ((net net) input &key (name "") (scale-factor 1.0) mean)
  "Set input blob for the network.

Arguments:
  NET - A NET object
  INPUT - Input blob (MAT from blob-from-image)
  :NAME - Input layer name
  :SCALE-FACTOR - Scale factor for input values (default 1.0)
  :MEAN - SCALAR with mean values to subtract"
  (%dnn-net-set-input (cvo-ptr net)
                      (cvo-ptr input)
                      name
                      (coerce scale-factor 'double-float)
                      (cvo-ptr (or mean (make-scalar)))))

@export
(defmethod net-set-input-shape ((net net) name shape)
  "Set input shape for a network layer.

Arguments:
  NET - A NET object
  NAME - Input layer name
  SHAPE - SHAPE MAT defining dimensions"
  (%dnn-net-set-input-shape (cvo-ptr net) name (cvo-ptr shape)))

@export
(defmethod net-forward ((net net) &optional (output-name ""))
  "Run forward pass to compute output of named layer.

Arguments:
  NET - A NET object
  OUTPUT-NAME - Name of the output layer (default \"\" for first output)

Returns:
  A MAT containing the network output."
  ;; prevents FLOATING-POINT-INVALID-OPERATION or segfault (if TBB enabled)
  (float-features:with-float-traps-masked (:invalid)
    (make-instance 'mat
                   :ptr (%dnn-net-forward (cvo-ptr net) output-name))))
