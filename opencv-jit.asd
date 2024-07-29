(defsystem opencv-jit
  :version "0.1.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :description "Bindings for OpenCV"
  :homepage "https://github.com/Junker/opencv-jit"
  :source-control (:git "https://github.com/Junker/opencv-jit.git")
  :depends-on ("uiop" "cxx-jit" "cl-annot" "trivial-garbage" "trivial-types")
  :components ((:module "src"
                :components ((:file "foreign")
                             (:file "util")
                             (:file "core")
                             (:file "imgcodecs")
                             (:file "imgproc")
                             (:file "highgui")
                             (:file "opencv-jit")
                             (:file "dnn"))))
  :in-order-to ((test-op (test-op :opencv-jit-test))))
