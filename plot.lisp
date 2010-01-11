(in-package :cl-asy)

(defun if-number-convert-to-string (x)
  (if (stringp x)
      x
      (write-to-string x)))

(defun array-as-string (arr)
  (reduce #'(lambda (x y)
	      (let ((x-str (if-number-convert-to-string x))
		    (y-str (if-number-convert-to-string y)))
		(concatenate 'string x-str ", " y-str)))
	  arr))

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

(defun generate-histo-plot-command (x-var y-var)
  (concatenate 'string
	       "histogram(" x-var ", " y-var ", nullpen, black, false);"))

(defmethod histo1d-plot ((histo histo1d))
  (with-slots (name binning) histo
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
				   (generate-histo-plot-command "x" "y"))))))

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

