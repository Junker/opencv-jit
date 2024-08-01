(defpackage opencv-jit/foreign
  (:use #:cl)
  (:import-from #:cxx-jit)
  (:import-from #:uiop
                #:strcat))
(in-package #:opencv-jit/foreign)

(cl-annot:enable-annot-syntax)

(defun from (includes funcs)
  (apply #'cxx-jit:from includes 'import
         (loop :for (func-name func) :on funcs :by #'cddr
               :collect (cons func
                              (string func-name))))
  (loop :for (func-name func) :on funcs :by #'cddr
        :do (export (intern (string func-name)))))

(let ((cxx-jit:*cxx-compiler-link-libs* (uiop:run-program "pkg-config --libs opencv4"
                                                          :output '(:string :stripped t)))
      (cxx-jit:*cxx-compiler-flags* (strcat "-std=c++17 -Wall "
                                            (uiop:run-program "pkg-config --cflags opencv4"
                                                              :output '(:string :stripped t))))
      ;; TEMP: until https://github.com/Islam0mar/CL-CXX-JIT/pull/9
      (cxx-jit:*cxx-type-name-to-cffi-type-symbol-alist* (append cxx-jit:*cxx-type-name-to-cffi-type-symbol-alist*
                                                                 (list (cons "size_t" :size)
                                                                       (cons "ssize_t" :ssize)))))
  (from '("<opencv2/opencv.hpp>" "<opencv2/core/mat.hpp>" "<opencv2/dnn/dnn.hpp>")
        ;; Cv
        '(;; ======== ImgProc
          :%cvt-color "[](cv::Mat *src, cv::Mat *dst, int code){return cv::cvtColor(*src, *dst, code);}"
          :%equalize-hist "[](cv::Mat *src, cv::Mat *dst){cv::equalizeHist(*src, *dst);}"
          :%compare-hist "[](cv::Mat *hist1, cv::Mat *hist2, int method){return cv::compareHist(*hist1, *hist2, method);}"
          :%bilateral-filter "[](cv::Mat *src, cv::Mat *dst, int d, double sc, double ss){cv::bilateralFilter(*src, *dst, d, sc, ss);}"
          :%resize "[](cv::Mat *src, cv::Mat *dst, cv::Size *dsize, int interpolation){cv::resize(*src, *dst, *dsize, interpolation);}"
          ;; ======== ImgCodecs
          :%imdecode "[](uchar *buf, size_t size, int flags){return new cv::Mat(cv::imdecode(std::vector<uchar>(buf, buf + size), flags));}"
          :%imread "[](std::string filename, int flags){return new cv::Mat(cv::imread(filename, flags));}"
          :%imwrite "[](std::string filename, cv::Mat *img, int *params, uint params_size){return cv::imwrite(filename, *img, std::vector<int>(params, params + params_size));}"
          ;; ======== HighGUI
          :%imshow "[](std::string wname, cv::Mat *mat){cv::imshow(wname, *mat);}"
          :%waitkey "[](int delay = 0){return cv::waitKey(delay);}"
          :%named-window "[](std::string wname, int flags){cv::namedWindow(wname, flags);}"
          :%destroy-window "[](std::string wname){cv::destroyWindow(wname);}"
          :%move-window "[](std::string wname, int x, int y){cv::moveWindow(wname, x, y);}"
          :%resize-window "[](std::string wname, int w, int h){cv::resizeWindow(wname, w, h);}"
          :%set-window-title "[](std::string wname, std::string title){cv::setWindowTitle(wname, title);}"
          ;; ======== Vec
          :%make-vec-int10 "[](int i0,int i1,int i2,int i3,int i4,int i5,int i6,int i7,int i8,int i9){return new cv::Vec<int,10>(i0,i1,i2,i3,i4,i5,i6,i7,i8,i9);}"
          :%vec-uchar-val "[](cv::Vec<uchar,10> *vec, int i){return vec->val[i];}"
          :%vec-schar-val "[](cv::Vec<schar,10> *vec, int i){return vec->val[i];}"
          :%vec-double-val "[](cv::Vec<double,10> *vec, int i){return vec->val[i];}"
          :%vec-float-val "[](cv::Vec<float,10> *vec, int i){return vec->val[i];}"
          :%vec-int-val "[](cv::Vec<int,10> *vec, int i){return vec->val[i];}"
          :%vec-short-val "[](cv::Vec<short,10> *vec, int i){return vec->val[i];}"
          :%vec-ushort-val "[](cv::Vec<ushort,10> *vec, int i){return vec->val[i];}"
          :%vec-uchar2-delete "[](cv::Vec2b *vec){delete vec;}"
          :%vec-uchar3-delete "[](cv::Vec3b *vec){delete vec;}"
          :%vec-uchar4-delete "[](cv::Vec4b *vec){delete vec;}"
          :%vec-schar2-delete "[](cv::Vec<schar, 2> *vec){delete vec;}"
          :%vec-schar3-delete "[](cv::Vec<schar, 3> *vec){delete vec;}"
          :%vec-schar4-delete "[](cv::Vec<schar, 4> *vec){delete vec;}"
          :%vec-double2-delete "[](cv::Vec2d *vec){delete vec;}"
          :%vec-double3-delete "[](cv::Vec3d *vec){delete vec;}"
          :%vec-double4-delete "[](cv::Vec4d *vec){delete vec;}"
          :%vec-float2-delete "[](cv::Vec2f *vec){delete vec;}"
          :%vec-float3-delete "[](cv::Vec3f *vec){delete vec;}"
          :%vec-float4-delete "[](cv::Vec4f *vec){delete vec;}"
          :%vec-int2-delete "[](cv::Vec2i *vec){delete vec;}"
          :%vec-int3-delete "[](cv::Vec3i *vec){delete vec;}"
          :%vec-int4-delete "[](cv::Vec4i *vec){delete vec;}"
          :%vec-int10-delete "[](cv::Vec<int,10> *vec){delete vec;}"
          :%vec-short2-delete "[](cv::Vec2s *vec){delete vec;}"
          :%vec-short3-delete "[](cv::Vec3s *vec){delete vec;}"
          :%vec-short4-delete "[](cv::Vec4s *vec){delete vec;}"
          :%vec-ushort2-delete "[](cv::Vec2w *vec){delete vec;}"
          :%vec-ushort3-delete "[](cv::Vec3w *vec){delete vec;}"
          :%vec-ushort4-delete "[](cv::Vec4w *vec){delete vec;}"
          ;; ======== Size
          :%new-size "[](){return new cv::Size();}"
          :%new-size-from-size "[](cv::Size *sz){return new cv::Size(*sz);}"
          :%new-size-wh "[](int width, int height){return new cv::Size2i(width, height);}"
          :%size-width "[](cv::Size *sz){return sz->width;}"
          :%size-height "[](cv::Size *sz){return sz->height;}"
          :%size-delete "[](cv::Size *sz){delete sz;}"
          ;; ======== Scalar
          :%new-scalar "[](){return new cv::Scalar();}"
          :%new-scalar4 "[](double v0, double v1, double v2, double v3){return new cv::Scalar(v0,v1,v2,v3);}"
          :%scalar-delete "[](cv::Scalar *scr){delete scr;}"
          ;; ======== Mat
          :%new-mat "[](){return new cv::Mat();}"
          :%new-mat-rows-cols-type "[](int rows, int cols, int type){return new cv::Mat(rows,cols,type);}"
          :%new-mat-rows-cols-type-s "[](int rows, int cols, int type, cv::Scalar *s){return new cv::Mat(rows,cols,type,*s);}"
          :%new-mat-from-mat "[](cv::Mat *mat){return new cv::Mat(*mat);}"
          :%new-mat-from-scalar "[](cv::Scalar *scr){return new cv::Mat(*scr);}"
          :%mat-abs-diff "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst){return cv::absdiff(*src1, *src2, *dst);}"
          :%mat-add "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst){return cv::add(*src1, *src2, *dst);}"
          :%mat-bitwise-and "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst){return cv::bitwise_and(*src1, *src2, *dst);}"
          :%mat-bitwise-and-with-mask "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst, cv::Mat *mask){return cv::bitwise_and(*src1, *src2, *dst, *mask);}"
          :%mat-bitwise-not "[](cv::Mat *src, cv::Mat *dst){return cv::bitwise_not(*src, *dst);}"
          :%mat-bitwise-not-with-mask "[](cv::Mat *src, cv::Mat *dst, cv::Mat *mask){return cv::bitwise_not(*src, *dst, *mask);}"

          :%mat-bitwise-or "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst){return cv::bitwise_or(*src1, *src2, *dst);}"
          :%mat-bitwise-or-with-mask "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst, cv::Mat *mask){return cv::bitwise_or(*src1, *src2, *dst, *mask);}"
          :%mat-bitwise-xor "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst){return cv::bitwise_xor(*src1, *src2, *dst);}"
          :%mat-bitwise-xor-with-mask "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst, cv::Mat *mask){return cv::bitwise_xor(*src1, *src2, *dst, *mask);}"
          :%mat-check-range "[](cv::Mat *mat){return cv::checkRange(*mat);}"
          :%mat-compare "[](cv::Mat *src1, cv::Mat *src2, cv::Mat *dst, int ct){return cv::compare(*src1, *src2, *dst, ct);}"
          :%mat-count-non-zero "[](cv::Mat *src){return cv::countNonZero(*src);}"

          :%mat-eye "[](int rows, int cols, int type){cv::Mat* mat = new cv::Mat(rows, cols, type); *mat = cv::Mat::eye(rows, cols, type); return mat;}"
          :%mat-zeros "[](int rows, int cols, int type){cv::Mat* mat = new cv::Mat(rows, cols, type); *mat = cv::Mat::zeros(rows, cols, type); return mat;}"
          :%mat-ones "[](int rows, int cols, int type){cv::Mat* mat = new cv::Mat(rows, cols, type); *mat = cv::Mat::ones(rows, cols, type); return mat;}"
          :%mat-region "[](cv::Mat *mat, cv::Rect *rect){return new cv::Mat(*mat, *rect);}"
          ;; methods
          :%mat-at-uchar "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<uchar>(*atvec);}"
          :%mat-at-schar "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<schar>(*atvec);}"
          :%mat-at-ushort "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<ushort>(*atvec);}"
          :%mat-at-short "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<short>(*atvec);}"
          :%mat-at-int "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<int>(*atvec);}"
          :%mat-at-float "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<float>(*atvec);}"
          :%mat-at-double "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return mat->at<double>(*atvec);}"
          :%mat-at-uchar2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2b(mat->at<cv::Vec2b>(*atvec));}"
          :%mat-at-schar2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec<schar, 2>(mat->at<cv::Vec<schar, 2>>(*atvec));}"
          :%mat-at-ushort2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2w(mat->at<cv::Vec2w>(*atvec));}"
          :%mat-at-short2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2s(mat->at<cv::Vec2s>(*atvec));}"
          :%mat-at-int2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2i(mat->at<cv::Vec2i>(*atvec));}"
          :%mat-at-float2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2f(mat->at<cv::Vec2f>(*atvec));}"
          :%mat-at-double2 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec2d(mat->at<cv::Vec2d>(*atvec));}"
          :%mat-at-uchar3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3b(mat->at<cv::Vec3b>(*atvec));}"
          :%mat-at-schar3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec<schar, 3>(mat->at<cv::Vec<schar, 3>>(*atvec));}"
          :%mat-at-ushort3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3w(mat->at<cv::Vec3w>(*atvec));}"
          :%mat-at-short3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3s(mat->at<cv::Vec3s>(*atvec));}"
          :%mat-at-int3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3i(mat->at<cv::Vec3i>(*atvec));}"
          :%mat-at-float3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3f(mat->at<cv::Vec3f>(*atvec));}"
          :%mat-at-double3 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec3d(mat->at<cv::Vec3d>(*atvec));}"
          :%mat-at-uchar4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4b(mat->at<cv::Vec4b>(*atvec));}"
          :%mat-at-schar4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec<schar, 4>(mat->at<cv::Vec<schar, 4>>(*atvec));}"
          :%mat-at-ushort4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4w(mat->at<cv::Vec4w>(*atvec));}"
          :%mat-at-short4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4s(mat->at<cv::Vec4s>(*atvec));}"
          :%mat-at-int4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4i(mat->at<cv::Vec4i>(*atvec));}"
          :%mat-at-float4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4f(mat->at<cv::Vec4f>(*atvec));}"
          :%mat-at-double4 "[](cv::Mat *mat, cv::Vec<int,10> *atvec){return new cv::Vec4d(mat->at<cv::Vec4d>(*atvec));}"
          :%mat-empty "[](cv::Mat *mat){return mat->empty();}"
          :%mat-clone "[](cv::Mat *mat){return new cv::Mat(mat->clone());}"
          :%mat-channels "[](cv::Mat *mat){return mat->channels();}"
          :%mat-col "[](cv::Mat *mat, int x){return new cv::Mat(mat->col(x));}"
          :%mat-cols "[](cv::Mat *mat){return mat->cols;}"
          :%mat-copy-to "[](cv::Mat *mat, cv::Mat *dst, cv::Mat *mask){return mat->copyTo(*dst, *mask);}"
          :%mat-convert-to "[](cv::Mat *mat, cv::Mat *dst, int type){return mat->convertTo(*dst, type);}"
          :%mat-elem-size "[](cv::Mat *mat){return mat->elemSize();}"
          :%mat-data "[](cv::Mat *mat){return mat->data;}"
          :%mat-depth "[](cv::Mat *mat){return mat->depth();}"
          :%mat-dims "[](cv::Mat *mat){return mat->dims;}"
          :%mat-inv "[](cv::Mat *mat){return mat->inv();}"
          :%mat-release "[](cv::Mat *mat){return mat->release();}"
          :%mat-reshape "[](cv::Mat *mat, int cn, int rows){return new cv::Mat(mat->reshape(cn, rows));}"
          :%mat-row "[](cv::Mat *mat, int x){return new cv::Mat(mat->row(x));}"
          :%mat-rows "[](cv::Mat *mat){return mat->rows;}"
          :%mat-size "[](cv::Mat *mat){return new cv::Size(mat->size());}"
          :%mat-axis-length "[](cv::Mat *mat, int axis){return mat->size[axis];}"
          :%mat-step "[](cv::Mat *mat){return mat->step;}"
          :%mat-t "[](cv::Mat *mat){return new cv::Mat(mat->t());}"
          :%mat-type "[](cv::Mat *mat){return mat->type();}"
          :%mat-total "[](cv::Mat *mat){return mat->total();}"
          ;; ======== Point
          :%new-point "[](){return new cv::Point();}"
          :%new-point-xy "[](int x, int y){return new cv::Point(x,y);}"
          :%point-x "[](cv::Point *pt){return pt->x;}"
          :%point-y "[](cv::Point *pt){return pt->y;}"
          :%point-cross "[](cv::Point *pt, cv::Point *pt2){return pt->cross(*pt2);}"
          :%point-ddot "[](cv::Point *pt, cv::Point *pt2){return pt->ddot(*pt2);}"
          :%point-dot "[](cv::Point *pt, cv::Point *pt2){return pt->dot(*pt2);}"
          :%point-inside "[](cv::Point *pt, cv::Rect *rect){return pt->inside(*rect);}"
          :%point-delete "[](cv::Point *pt){delete pt;}"
          ;; ======== Rect
          :%new-rect "[](){return new cv::Rect();}"
          :%new-rect-xywh "[](int x, int y, int w, int h){return new cv::Rect(x,y,w,h);}"
          :%rect-x "[](cv::Rect *rect){return rect->x;}"
          :%rect-y "[](cv::Rect *rect){return rect->y;}"
          :%rect-width "[](cv::Rect *rect){return rect->width;}"
          :%rect-height "[](cv::Rect *rect){return rect->height;}"
          :%rect-area "[](cv::Rect *rect){return rect->area();}"
          :%rect-br "[](cv::Rect *rect){return rect->br();}"
          :%rect-empty "[](cv::Rect *rect){return rect->empty();}"
          :%rect-contains "[](cv::Rect *rect, cv::Point *pt){return rect->contains(*pt);}"
          :%rect-size "[](cv::Rect *rect){return new cv::Size(rect->size());}"
          :%rect-delete "[](cv::Rect *rect){delete rect;}"
          ;; ======== DNN
          :%dnn-read-net "[](std::string model, std::string config){return cv::dnn::Net(cv::dnn::readNet(model, config));}"
          :%dnn-read-net-from-caffe "[](std::string prototxt, std::string model){return new cv::dnn::Net(cv::dnn::readNetFromCaffe(prototxt, model));}"
          :%dnn-read-net-from-darknet "[](std::string cfgfile, std::string model){return cv::dnn::Net(cv::dnn::readNetFromDarknet(cfgfile, model));}"
          :%dnn-read-net-from-onnx "[](std::string onnxfile){return new cv::dnn::Net(cv::dnn::readNetFromONNX(onnxfile));}"
          :%dnn-read-net-from-tensorflow "[](std::string model, std::string config){return new cv::dnn::Net(cv::dnn::readNetFromTensorflow(model, config));}"
          :%dnn-read-net-from-model-optimizer "[](std::string xml, std::string bin){return new cv::dnn::Net(cv::dnn::readNetFromModelOptimizer(xml, bin));}"
          :%dnn-read-net-from-tflite "[](std::string model){return new cv::dnn::Net(cv::dnn::readNetFromTFLite(model));}"
          :%dnn-read-net-from-torch "[](std::string model, bool binary, bool evaluate){return new cv::dnn::Net(cv::dnn::readNetFromTorch(model, binary, evaluate));}"
          :%dnn-blob-from-image "[](cv::Mat *image, double scale_factor, cv::Size *sz, cv::Scalar *mean, bool swap_rb, bool crop){cv::Mat *output = new cv::Mat(); cv::dnn::blobFromImage(*image, *output, scale_factor, *sz, *mean, swap_rb, crop); return output;}"
          ;; methods
          :%dnn-net-dump "[](cv::dnn::Net *net){return net->dump();}"
          :%dnn-net-empty "[](cv::dnn::Net *net){return net->empty();}"
          :%dnn-net-enable-fusion "[](cv::dnn::Net *net, bool fusion){net->enableWinograd(fusion);}"
          :%dnn-net-enable-winograd "[](cv::dnn::Net *net, bool use_winograd){net->enableWinograd(use_winograd);}"
          :%dnn-net-delete "[](cv::dnn::Net *net){delete net;}"
          :%dnn-net-forward "[](cv::dnn::Net *net, std::string name){return new cv::Mat(net->forward(name));}"
          :%dnn-net-set-input "[](cv::dnn::Net *net, cv::Mat *blob, std::string name, double scale, cv::Scalar *mean){net->setInput(*blob, name, scale, *mean);}"
          :%dnn-net-set-input-shape "[](cv::dnn::Net *net, std::string name, cv::Mat *shape){net->setInputShape(name, *shape);}"

          ;; ======== FaceDetectorYN
          :%face-detector-yn-create "[](std::string model, std::string config, cv::Size *input_size, float score_threashold, float nms_threshold, int top_k, int backend_id, int target_id){return cv::FaceDetectorYN::create(model,config,*input_size,score_threashold,nms_threshold,top_k,backend_id, target_id);}"
          :%face-detector-yn-detect "[](cv::FaceDetectorYN *fyn, cv::Mat *image){cv::Mat *faces = new cv::Mat(); fyn->detect(*image,*faces); return faces;}"
          :%face-detector-yn-set-input-size "[](cv::FaceDetectorYN *fyn, cv::Size *input_size){return fyn->setInputSize(*input_size);}"
          :%face-detector-yn-delete "[](cv::FaceDetectorYN *fyn){delete fyn;}")))
