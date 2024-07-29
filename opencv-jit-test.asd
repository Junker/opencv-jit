(defsystem opencv-jit-test
  :version "0.1.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :description "Tests for OpenCV-JIT"
  :homepage "https://github.com/Junker/opencv-jit"
  :serial T
  :components ((:file "test/test"))
  :depends-on (:opencv-jit :parachute :alexandria)
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :opencv-jit-test)))
