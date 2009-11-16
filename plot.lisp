(defpackage :cl-asy
  (:use :common-lisp)
  (:export :plot))

(in-package :cl-asy)

(defclass bin1d ()
  ((xmin :initarg :xmin :initform (error "Minimum edge required.."))
   (xmax :initarg :xmax :initform (error "Maximum edge required.."))
   (content :initarg :content :initform 0.0)))

(defmethod bin1d-fill ((bin bin1d) value weight)
  (with-slots (xmin xmax content) bin
    (if (and (>= value xmin) (< xmax))
	(setf content (+ content weight))
	(error "Value is outside the bin!"))))

(defun create-linear-binning (nbins xmin xmax)
  (let* ((width (/ (- xmax xmin) nbins))
	 (right-edge (+ xmin width)))
    (if (> nbins 1)
	(append (list (make-instance 'bin1d :xmin xmin :xmax right-edge))
		(create-linear-binning (- nbins 1) right-edge xmax))
	(list (make-instance 'bin1d :xmin xmin :xmax xmax)))))

(defclass histo1d ()
  ((name :initarg :name :initform (error "Histogram needs a name"))
   (bins :initarg :bins :initform (error "Histogram needs number of bins"))
   (xmin :initarg :xmin :initform (error "Histogram needs minimum x value"))
   (xmax :initarg :xmax :initform (error "Histogram needs maximum x value"))
   (binning :initarg :binning)))

(defun translate-to-asy (&body 

(defun plot (p)
  nil)

