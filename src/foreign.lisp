(defpackage opencv-jit/foreign
  (:use #:cl)
  (:import-from #:cxx-jit)
  (:import-from #:uiop
                #:strcat))
(in-package #:opencv-jit/foreign)

(setf cxx-jit:*cxx-compiler-link-libs* (uiop:run-program "pkg-config --libs opencv4"
                                                         :output '(:string :stripped t)))

(setf cxx-jit:*cxx-compiler-flags* (strcat "-std=c++17 -Wall "
                                           (uiop:run-program "pkg-config --cflags opencv4"
                                                             :output '(:string :stripped t))))

(defun from (includes funcs)
  (apply #'cxx-jit:from includes 'import
         (loop :for (func-name func) :on funcs :by #'cddr
               :collect (cons func
                              (string func-name))))
  (loop :for (func-name func) :on funcs :by #'cddr
        :do (export (intern (string func-name)))))

(from '("<opencv2/opencv.hpp>" "<opencv2/core/mat.hpp>" "<opencv2/dnn/dnn.hpp>")
      ;; Cv
      '(;; ======== ImgProc
        :%cvt-color "[](cv::Mat *src, cv::Mat *dst, int code){return cv::cvtColor(*src, *dst, code);}"
        :%equalize-hist "[](cv::Mat *src, cv::Mat *dst){cv::equalizeHist(*src, *dst);}"
        :%compare-hist "[](cv::Mat *hist1, cv::Mat *hist2, int method){return cv::compareHist(*hist1, *hist2, method);}"
        :%bilateral-filter "[](cv::Mat *src, cv::Mat *dst, int d, double sc, double ss){cv::bilateralFilter(*src, *dst, d, sc, ss);}"
        :%resize "[](cv::Mat *src, cv::Mat *dst, cv::Size *dsize, int interpolation){cv::resize(*src, *dst, *dsize, interpolation);}"
        ;; ======== ImgCodecs
        :%imread "[](std::string filename, int flags){return new cv::Mat(cv::imread(filename, flags));}"
        :%imwrite "[](std::string filename, cv::Mat *img){return cv::imwrite(filename, *img);}"
        ;; ======== HighGUI
        :%imshow "[](std::string wname, cv::Mat *mat){cv::imshow(wname, *mat);}"
        :%waitkey "[](int delay = 0){return cv::waitKey(delay);}"
        :%window-new "[](std::string wname, int flags){cv::namedWindow(wname, flags);}"
        :%window-close "[](std::string wname){cv::destroyWindow(wname);}"
        :%window-move "[](std::string wname, int x, int y){cv::moveWindow(wname, x, y);}"
        :%window-resize "[](std::string wname, int w, int h){cv::resizeWindow(wname, w, h);}"
        :%window-set-title "[](std::string wname, std::string title){cv::setWindowTitle(wname, title);}"
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
        :%scalar-val "[](cv::Scalar *scr, int i){return scr->val[i];}"
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
        :%mat-at-1d-uchar "[](cv::Mat *mat, int i){return mat->at<uchar>(i);}"
        :%mat-at-1d-schar "[](cv::Mat *mat, int i){return mat->at<schar>(i);}"
        :%mat-at-1d-ushort "[](cv::Mat *mat, int i){return mat->at<ushort>(i);}"
        :%mat-at-1d-short "[](cv::Mat *mat, int i){return mat->at<short>(i);}"
        :%mat-at-1d-int "[](cv::Mat *mat, int i){return mat->at<int>(i);}"
        :%mat-at-1d-float "[](cv::Mat *mat, int i){return mat->at<float>(i);}"
        :%mat-at-1d-double "[](cv::Mat *mat, int i){return mat->at<double>(i);}"
        :%mat-at-2d-uchar "[](cv::Mat *mat, int row, int col){return mat->at<uchar>(row,col);}"
        :%mat-at-2d-schar "[](cv::Mat *mat, int row, int col){return mat->at<schar>(row,col);}"
        :%mat-at-2d-ushort "[](cv::Mat *mat, int row, int col){return mat->at<ushort>(row,col);}"
        :%mat-at-2d-short "[](cv::Mat *mat, int row, int col){return mat->at<short>(row,col);}"
        :%mat-at-2d-int "[](cv::Mat *mat, int row, int col){return mat->at<int>(row,col);}"
        :%mat-at-2d-float "[](cv::Mat *mat, int row, int col){return mat->at<float>(row,col);}"
        :%mat-at-2d-double "[](cv::Mat *mat, int row, int col){return mat->at<double>(row,col);}"
        :%mat-at-3d-uchar "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<uchar>(i0,i1,i2);}"
        :%mat-at-3d-schar "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<schar>(i0,i1,i2);}"
        :%mat-at-3d-ushort "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<ushort>(i0,i1,i2);}"
        :%mat-at-3d-short "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<short>(i0,i1,i2);}"
        :%mat-at-3d-int "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<int>(i0,i1,i2);}"
        :%mat-at-3d-float "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<float>(i0,i1,i2);}"
        :%mat-at-3d-double "[](cv::Mat *mat, int i0, int i1, int i2){return mat->at<double>(i0,i1,i2);}"
        :%mat-empty "[](cv::Mat *mat){return mat->empty();}"
        :%mat-clone "[](cv::Mat *mat){return new cv::Mat(mat->clone());}"
        :%mat-channels "[](cv::Mat *mat){return mat->channels();}"
        :%mat-col "[](cv::Mat *mat, int x){return new cv::Mat(mat->col(x));}"
        :%mat-cols "[](cv::Mat *mat){return mat->cols;}"
        :%mat-copy-to "[](cv::Mat *mat, cv::Mat *dst, cv::Mat *mask){return mat->copyTo(*dst, *mask);}"
        :%mat-convert-to "[](cv::Mat *mat, cv::Mat *dst, int type){return mat->convertTo(*dst, type);}"
        :%mat-elem-size "[](cv::Mat *mat){return mat->elemSize();}"
        :%mat-depth "[](cv::Mat *mat){return mat->depth();}"
        :%mat-inv "[](cv::Mat *mat){return mat->inv();}"
        :%mat-release "[](cv::Mat *mat){return mat->release();}"
        :%mat-reshape "[](cv::Mat *mat, int cn, int rows){return new cv::Mat(mat->reshape(cn, rows));}"
        :%mat-row "[](cv::Mat *mat, int x){return new cv::Mat(mat->row(x));}"
        :%mat-rows "[](cv::Mat *mat){return mat->rows;}"
        :%mat-size "[](cv::Mat *mat){return new cv::Size(mat->size());}"
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
        :%dnn-net-delete "[](cv::dnn::Net *net){delete net;}"
        :%dnn-net-forward "[](cv::dnn::Net *net, std::string name){return new cv::Mat(net->forward(name));}"
        :%dnn-net-set-input "[](cv::dnn::Net *net, cv::Mat input, std::string name, double scale, cv::Scalar mean){return net->setInput(input, name, scale, mean);}"
        ;; ======== FaceDetectorYN
        :%face-detector-yn-create "[](std::string model, std::string config, cv::Size *input_size, float score_threashold, float nms_threshold, int top_k, int backend_id, int target_id){return cv::FaceDetectorYN::create(model,config,*input_size,score_threashold,nms_threshold,top_k,backend_id, target_id);}"
        :%face-detector-yn-detect "[](cv::FaceDetectorYN *fyn, cv::Mat *image){cv::Mat *faces = new cv::Mat(); fyn->detect(*image,*faces); return faces;}"
        :%face-detector-set-input-size "[](cv::FaceDetectorYN *fyn, cv::Size *input_size){return fyn->setInputSize(*input_size);}"))
