(defpackage #:opencv-jit-test
  (:use #:cl #:parachute #:opencv-jit))

(in-package #:opencv-jit-test)

(defvar *tux8-png-data*
  (coerce #(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 8 0 0 0 8 8 2 0 0 0 75
            109 41 220 0 0 0 123 73 68 65 84 8 215 99 96 128 1 38 38 150 128 128 64 6 76
            160 175 111 192 202 202 14 148 70 151 120 115 136 231 198 28 14 22 22 54 20
            81 160 194 31 63 190 253 253 251 27 172 131 17 33 1 20 130 35 33 33 97 144 82
            32 126 177 129 27 89 247 247 239 63 128 36 203 215 189 60 64 138 149 149 243
            247 239 239 64 198 209 137 226 207 55 136 252 249 35 200 180 105 231 207 183
            47 127 191 217 204 6 148 3 162 231 207 191 159 58 246 74 196 251 9 0 196 126
            50 206 63 197 30 51 0 0 0 0 73 69 78 68 174 66 96 130)
          '(vector (unsigned-byte 8))))

(defvar *tux8-bmp-data*
  (coerce #(66 77 246 0 0 0 0 0 0 0 54 0 0 0 40 0 0 0 8 0 0 0 8 0 0 0 1 0 24 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5 118 167 7 166 226 10 181 238 4 2 2 4
            2 2 9 117 169 9 127 186 0 0 0 12 189 245 12 189 245 4 2 2 251 253 253 251 253
            253 18 142 194 31 185 240 28 181 236 0 0 0 11 176 232 251 253 253 251 253 253
            251 253 253 251 253 253 11 9 9 0 0 0 0 0 0 0 0 0 251 253 253 251 253 253 251
            253 253 251 253 253 19 18 18 0 0 0 0 0 0 0 0 0 4 2 2 246 248 248 251 253 253
            4 2 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 12 194 236 8 156 216 6 4 4 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 48 47 47 7 5 5 4 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 2 2 81
            80 80 0 0 0 0 0 0 0 0 0)
          '(vector (unsigned-byte 8))))

(define-test size-create
    (let ((s (make-size 10 20)))
      (is = (size-width s) 10)
      (is = (size-height s) 20)))

(define-test scalar-create
    (let ((s (make-scalar 10 20 30 40)))
      (is = (scalar-val s 0) 10)
      (is = (scalar-val s 3) 40)))

(define-test rect-create
    (let ((r (make-rect 10 20 50 100)))
      (is = (rect-width r) 50)
      (is = (rect-y r) 20)))

(define-test point-create
    (let ((p (make-point 10 20)))
      (is = (point-x p) 10)
      (is = (point-y p) 20)))

(define-test imdecode
    (let* ((mat (imdecode *tux8-png-data*))
           (size (mat-size mat)))
      (is = (mat-total mat) 64)
      (is = (mat-rows mat) 8)
      (is = (mat-cols mat) 8)
      (is = (size-width size) 8)
      (is = (size-height size) 8)
      (is = (mat-channels mat) 3)
      (is = (mat-dims mat) 2)
      (is eq (mat-depth mat) :CV-8U)))

(define-test imread
    (uiop:with-temporary-file (:stream stream
                               :pathname path
                               :type "png"
                               :direction :output
                               :element-type '(unsigned-byte 8))
      (write-sequence *tux8-png-data* stream)
      (finish-output stream)
      (let* ((mat (imread path))
             (size (mat-size mat)))
        (is = (mat-total mat) 64)
        (is = (mat-rows mat) 8)
        (is = (mat-cols mat) 8)
        (is = (size-width size) 8)
        (is = (size-height size) 8)
        (is = (mat-channels mat) 3)
        (is = (mat-dims mat) 2)
        (is eq (mat-depth mat) :CV-8U))))

(define-test imwrite
    (uiop:with-temporary-file (:pathname path
                               :type "bmp"
                               :element-type '(unsigned-byte 8))
      (imwrite path (imdecode *tux8-png-data*))
      (is equalp
          (alexandria:read-file-into-byte-vector path)
          *tux8-bmp-data*)))

(define-test mat-at
    (let* ((mat (imdecode *tux8-png-data* :IMREAD-GRAYSCALE)))
      (is = (mat-at mat 0 0) 0)
      (is = (mat-at mat 4 4) 252)))

(define-test vec-mat-at
    (let* ((mat (imdecode *tux8-png-data*))
           (v1 (mat-at mat 0 0))
           (v2 (mat-at mat 4 4)))
      (is = (vec-len v1) 3)
      (is eq (vec-type v1) :uchar)
      (is = (vec-val v1 0) 0)
      (is = (vec-val v1 1) 0)
      (is = (vec-val v2 0) 251)
      (is = (vec-val v2 1) 253)))
