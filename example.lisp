(require :cl-asy)
(require :alexandria)

(in-package :cl-user)

(defmacro with-asy-plot (file &rest body)
  `(with-open-file (*standard-output* ,file :direction :output
				      :if-exists :supersede)
     (format t "import graph;~%")
     (format t "import stats;~%")
     (format t "size(20cm, 20cm, IgnoreAspect);~%")
     (format t ,@body)
     (format t "xaxis(\"$x$\",BottomTop,LeftTicks);~%")
     (format t "yaxis(\"$dP/dx$\",LeftRight,RightTicks(trailingzero));~%")))

(defmacro mix-plot (&rest body)
  `(progn (concatenate 'string
		       ,@body)))

(defun plot-neutrons (x)
  (let ((h1 (make-instance 'cl-asy:histo1d
			   :name (concatenate 'string "neutronEnergies" (write-to-string x))
			   :bins 100
			   :xmin 0.0
			   :xmax 100.0)))
    (cl-asy:histo1d-fill h1 x 1.0)
    h1))

(defun test-cl-asy-plot1 ()
  (let ((h1 (plot-neutrons 10))
	(h2 (plot-neutrons 20))
	(g1 (make-instance 'cl-asy:datagraph2d)))
    (with-asy-plot #P"/home/mael/tmp/test.asy"
;;      (cl-asy:histo1d-plot h1))))
      (mix-plot (cl-asy:histo1d-plot h1)
		(cl-asy:histo1d-plot h2)))
    (cl-asy:histo1d-print h1)))
;;		(cl-asy:datagraph2d-plot g1)))))