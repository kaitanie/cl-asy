(in-package :cl-asy)

(defclass bin1d ()
  ((xmin :initarg :xmin :initform (error "Minimum edge required.."))
   (xmax :initarg :xmax :initform (error "Maximum edge required.."))
   (content :initarg :content :initform 0.0)))

(defmethod bin1d-xmin ((bin bin1d))
  (with-slots (xmin) bin
    xmin))

(defmethod bin1d-xmax ((bin bin1d))
  (with-slots (xmax) bin
    xmax))

(defmethod bin1d-middle ((bin bin1d))
  (with-slots (xmax xmin) bin
    (/ (+ xmax xmin) 2.0)))

(defmethod bin1d-content ((bin bin1d))
  (with-slots (content) bin
    content))

(defmethod bin1d-point ((bin bin1d))
  (let ((x-value (bin1d-middle bin))
	(y-value (bin1d-content bin)))
    '(x-value y-value)))


(defmethod bin1d-set-content ((bin bin1d) value)
  (with-slots (content) bin
    (setf content value)))

(defmethod bin1d-fill ((bin bin1d) value weight)
  (with-slots (xmin xmax content) bin
    (if (and (>= value xmin) (< xmax))
	(setf content (+ content weight))
	(error "Value is outside the bin!"))))

(defmethod bin1d-print ((bin bin1d))
  (with-slots (xmin xmax content) bin
    (format t (concatenate 'string "xmin = " (write-to-string xmin)
			   " xmax = " (write-to-string xmax)
			   " content = " (write-to-string content) "~%"))))

(defun create-linear-binning (nbins xmin xmax)
  (let* ((width (/ (- xmax xmin) nbins))
	 (right-edge (+ xmin width)))
    (if (> nbins 1)
	(append (list (make-instance 'bin1d :xmin xmin :xmax right-edge))
		(create-linear-binning (- nbins 1) right-edge xmax))
	(list (make-instance 'bin1d :xmin xmin :xmax xmax)))))

(defclass histo1d ()
  ((name :initarg :name :initform (error "Histogram needs a name"))
   (title :initarg :title :initform "Histogram title")
   (bins :initarg :bins :initform (error "Histogram needs number of bins"))
   (overflow :initform 0.0)
   (underflow :initform 0.0)
   (xmin :initarg :xmin :initform (error "Histogram needs minimum x value"))
   (xmax :initarg :xmax :initform (error "Histogram needs maximum x value"))
   (manual-binning :initarg :manual-binning :initform nil)
   (binning :initarg :binning :initform '())))

(defmethod initialize-instance :after ((histo histo1d) &key)
  (with-slots (bins xmin xmax manual-binning binning) histo
    (if (not manual-binning)
	(setf binning (create-linear-binning bins xmin xmax)))))

(defmethod histo1d-add-bin ((histo histo1d) bin)
  (with-slots (binning) histo
    (setf binning (append binning (list bin)))))

(defmethod histo1d-fill ((histo histo1d) value weight)
  (with-slots (xmin xmax overflow underflow) histo
    (cond ((< value xmin) (setf underflow (+ underflow weight)))
	  ((or (> value xmax) (eq value xmax)) (setf overflow (+ overflow weight)))
	  ( t
	   (with-slots (binning) histo
	     (dolist (bin binning)
	       (with-slots (xmin xmax) bin 
		 (if (and (>= value xmin) (< value xmax))
		     (bin1d-fill bin value weight)))))))))

(defmethod histo1d-print ((histo histo1d))
  (with-slots (binning underflow overflow) histo
    (dolist (bin binning)
      (bin1d-print bin))
    (format t (concatenate 'string "Underflow: " (write-to-string underflow) "~%"))
    (format t (concatenate 'string "Overflow: " (write-to-string overflow) "~%"))))

(defmethod histo1d-create-from-binning ((histo histo1d) x-mins x-maxs contents)
  (with-slots (binning) histo
	(setf binning (append binning
			      (list (make-instance 'bin1d 
						   :xmin (first x-mins)
						   :xmax (first x-maxs)
						   :content (first contents)))))
	(if (first (rest contents))
	    (histo1d-create-from-binning histo (rest x-mins) (rest x-maxs) (rest contents)))))
      
(defstruct bin-content
  (xmin 0.0)
  (xmax 0.0)
  (content 0.0))

;; Bin-content-list contains (xmin xmax content) for each bin
;; Data-entry for a histogram:
;; (defparameter *histo1* (make-histo-from-data "histo1" 10
;;                           (list (make-bin-content 0.0 0.1) (make-bin-content 0.1 0.2 0.0)
;;                           ...
(defun make-histo-from-data (name bins bin-content-list)
  (let ((histo (make-instance 'histo1d
			      :name name
			      :xmin (bin-content-xmin (first bin-content-list))
			      :xmax (bin-content-xmax (first (last bin-content-list)))
			      :bins bins
			      :manual-binning t)))
     (dolist (bin-data bin-content-list)
       (histo1d-add-bin histo (make-instance 'bin1d
					     :xmin (bin-content-xmin bin-data)
					     :xmax (bin-content-xmax bin-data)
					     :content (bin-content-content bin-data))))
     histo))

(defun create-histogram-from-binning (&key ((:name name)) ((:title title))
				      ((:nbins nbins)) ((:xmin xmin)) ((:xmax xmax))
				      ((:binning binning)))
  (make-instance 'histo1d :name name :title title
		 :xmin xmin :xmax xmax :bins nbins
		 :manual-binning t :binning binning))

(defun example-histo-from-binning ()
(create-histogram-from-binning :name "histo"
:title "My histogram title"
:nbins 10
:xmin 0.0
:xmax 1.0
:binning (list
(make-instance 'cl-asy:bin1d :xmin 0
:xmax 0.1
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.1
:xmax 0.2
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.2
:xmax 0.3
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.3
:xmax 0.4
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.4
:xmax 0.5
:content 1)
(make-instance 'cl-asy:bin1d :xmin 0.5
:xmax 0.6
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.6
:xmax 0.7
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.7
:xmax 0.8
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.8
:xmax 0.9
:content 0)
(make-instance 'cl-asy:bin1d :xmin 0.9
:xmax 1
:content 0)
) ;; Close the list of bins
) ;; Close create-histogram-from-binning
)
