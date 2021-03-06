(in-package :cl-asy)

(defmacro with-asy-plot (file (&key legend) &rest body)
  `(with-open-file (*standard-output* ,file :direction :output
				      :if-exists :supersede)
     (format t "import graph;~%")
     (format t "import stats;~%")
     (format t "size(20cm, 20cm, IgnoreAspect);~%")
     (format t ,@body)
     (format t "xaxis(\"$x$\",BottomTop,LeftTicks);~%")
     (format t "yaxis(\"$dP/dx$\",LeftRight,RightTicks(trailingzero));~%")
     (if ,legend (format t "add(legend(),point(NW),SE,UnFill);~%"))))

(defmacro mix-plot (&rest body)
  `(progn (concatenate 'string
		       ,@body)))

(defun array-as-string (array)
  (let ((array-str (mapcar #'write-to-string array)))
    (reduce #'(lambda (x y)
		(concatenate 'string x ", " y))
	    array-str)))

(defun generate-array-definition (variable type data)
  (concatenate 'string type " " variable " = {"
			 (array-as-string data) "};"
			 "// Array of length " (write-to-string (list-length data)) "~%"))

(defmacro generate-function (name return-type list-of-args &rest body)
  `(concatenate 'string
		,return-type " ",name "()~%"
		"{~%"
		,@body
		"}~%"))

(defmacro generate-and-call-function (name return-type list-of-args &rest body)
  `(concatenate 'string
		(generate-function ,name ,return-type ,list-of-args ,@body)
		,name "();~%"))

(defun generate-histo-plot-command (x-var y-var title)
  (let ((command (concatenate 'string
			      "histogram(" x-var ", " y-var ", nullpen, black, false")))
    (if (not (null title))
	(concatenate 'string command ", " title))
    (concatenate 'string command ");~%")))

(defmethod histo1d-gnuplot ((histo histo1d))
  (with-slots (name title binning) histo
    (let ((points (mapcar #'bin1d-point binning)))
      (clnuplot:make-plot :lines-points
			  points
			   :pointsize 2.0
			   :linewidth 3.0
			   :filename name
			   :xlabel "X"
			   :ylabel "Y"
			   :x-coord #'first
			   :y-coord #'second
			   :title title
			   :ymin 0.0))))

(defmethod histo1d-plot ((histo histo1d))
  (with-slots (name title binning) histo
    (let ((x-array (append (list (bin1d-xmin (first binning)))
			   (mapcar #'bin1d-xmax binning)))
	  (contents-array (mapcar #'bin1d-content binning)))
      (generate-and-call-function (concatenate
				   'string
				   "plot_" name)
				  "void"
				  '()
				  (concatenate
				   'string
				   (generate-array-definition "x" "real[]" x-array)
				   (generate-array-definition "y" "real[]" contents-array)
				   (generate-histo-plot-command "x" "y" title))))))

(defun generate-asy-header ()
  (concatenate 'string
	       "import graph;~%"
	       "import stats;~%"
	       "size(20cm, 20cm, IgnoreAspect);~%"))

(defun test-histo1 ()
  (let ((h1 (make-instance 'histo1d :name "h1" :xmin 0.0 :xmax 1.0 :bins 100)))
    (loop
	 for i from 1 to 1000
	 do (histo1d-fill h1 (random 1.0) 1.0))
    (histo1d-plot h1)))

(defun plot-histo1 (output-file)
  (with-open-file (*standard-output* output-file :direction :output
				       :if-exists :supersede)
    (format t (generate-asy-header))
    (format t (test-histo1))
    (format t "plot_h1();~%")
    (format t "xaxis(\"$x$\",BottomTop,LeftTicks);~%")
    (format t "yaxis(\"$dP/dx$\",LeftRight,RightTicks(trailingzero));~%")))

;;xaxis("$x$",BottomTop,LeftTicks);
;;yaxis("$dP/dx$",LeftRight,RightTicks(trailingzero));

