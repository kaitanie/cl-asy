(defpackage :cl-asy
  (:use :common-lisp)
  (:export :histo1d
	   :histo1d-fill
	   :histo1d-print
	   :create-linear-binning))

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

(defmethod bin1d-print ((bin bin1d))
  (with-slots (xmin xmax content) bin
    (format t (concatenate 'string "xmin = " (write-to-string xmin)
			   " xmax = " (write-to-string xmax)
			   " content = " (write-to-string content)))))

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
   (manual-binning :initarg :manual-binning :initform nil)
   (binning :initarg :binning :initform '())))

(defmethod initialize-instance :after ((histo histo1d) &key)
  (with-slots (bins xmin xmax manual-binning binning) histo
    (if (not manual-binning)
	(setf binning (create-linear-binning bins xmin xmax))
	(setf binning '()))))

(defmethod histo1d-fill ((histo histo1d) value weight)
  (with-slots (binning) histo
    (dolist (bin binning)
      (with-slots (xmin xmax) bin 
	(if (and (>= value xmin) (< value xmax))
	    (bin1d-fill bin value weight))))))

(defmethod histo1d-print ((histo histo1d))
  (with-slots (binning) histo
    (dolist (bin binning)
      (bin1d-print bin))))

(defmethod histo1d-create-from-binning ((histo histo1d) x-mins x-maxs contents)
  (with-slots (binning) histo
	(setf binning (append binning
			      (list (make-instance 'bin1d 
						   :xmin (first x-mins)
						   :xmax (first x-maxs)
						   :content (first contents)))))
	(if (first (rest contents))
	    (histo1d-create-from-binning histo (rest x-mins) (rest x-maxs) (rest contents)))))
      
(defun array-as-string (arr)
  (let ((str "")
	(len (list-length arr))
	(i 0))
    (dolist (item arr)
      (progn
	(setf i (+ i 1))
	(if (> i 1)
	    (setf str (concatenate 'string str ", " (write-to-string item)))
	    (setf str (concatenate 'string str (write-to-string item))))))
    str))

(defun generate-array-definition (variable type data)
  (format t (concatenate 'string type " " variable " = {"
			 (array-as-string data) "};")))

;;(defmethod histo1d-plot ((histo histo1d))
;;  nil)

;;(defun make-histo1d (&key 