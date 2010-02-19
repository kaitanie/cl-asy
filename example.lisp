(require :cl-asy)
(require :alexandria)

(in-package :cl-user)

(defmacro with-asy-plot (file (&key legend) &rest body)
  `(with-open-file (*standard-output* ,file :direction :output
				      :if-exists :supersede)
     (format t "import graph;~%")
     (format t "import stats;~%")
     (format t "size(20cm, 20cm, IgnoreAspect);~%")
     (format t ,@body)
     (format t "xaxis(\"$x$\",BottomTop,LeftTicks);~%")
     (format t "yaxis(\"$dP/dx$\",LeftRight,RightTicks(trailingzero));~%")
     (if ,legend (format t "add(pic,legend(pic),point(pic,NW),20SE,UnFill);~%"))))

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

(defun create-randoms (number-of-entries)
  (labels ((generate-randoms (acc n)
	     (if (eq n 0)
		 acc
		 (generate-randoms (push (random 1.0) acc) (- n 1)))))
    (generate-randoms '() number-of-entries)))
	
(defun create-random-histogram (number-of-entries)
  (let ((randoms (create-randoms number-of-entries))
	(histo (make-instance 'cl-asy:histo1d :name "randoms" :xmin -10.0 :xmax 10.0 :bins 100)))
    (dolist (item randoms)
      (cl-asy:histo1d-fill histo item 1.0))
    histo))

(defun create-gaussian-histogram (number-of-entries)
  (let ((x 0)
	(y 0)
	(histo (make-instance 'cl-asy:histo1d :name "gauss" :xmin -10.0 :xmax 10.0 :bins 100)))
    (labels ((collect-gaussian-randoms (acc number-of-randoms)
	       (if (or (< number-of-randoms 0) (eq number-of-randoms 0))
		   acc
		   (progn
		     (multiple-value-bind (x1 x2) (alexandria:gaussian-random)
		       (progn (push x1 acc) (push x2 acc)))
		     (collect-gaussian-randoms acc (- number-of-randoms 2))))))
      (dolist (item (collect-gaussian-randoms '() number-of-entries))
	(cl-asy:histo1d-fill histo item 1.0)))
    histo))

(defun test-cl-asy-random-histo ()
  (let ((histo (create-random-histogram 10000))
	(gauss (create-gaussian-histogram 10000)))
    (with-asy-plot #P"/home/mael/tmp/histo.asy"
		   (:legend t)
		   (mix-plot
;;		    (cl-asy:histo1d-plot histo)
		    (cl-asy:histo1d-plot gauss)))))

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