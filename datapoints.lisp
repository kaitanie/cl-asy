(in-package :cl-asy)

(defclass datapoint2d ()
  ((x
    :initarg :x
    :initform 0.0)
   (y
    :initarg :y
    :initform 0.0)
   (dx
    :initarg :dx
    :initform 0.0)
   (dy
    :initarg :dy
    :initform 0.0)))

(defmethod datapoint2d-x ((point datapoint2d))
  (with-slots (x) point
    x))

(defmethod datapoint2d-y ((point datapoint2d))
  (with-slots (y) point
    y))

(defmethod datapoint2d-print ((point datapoint2d))
  (with-slots (x y dx dy) point
    (format t (concatenate 'string
			   "(x = " (write-to-string x) "+-" (write-to-string dx) ","
			   " y = " (write-to-string y) "+-" (write-to-string dy) ")~%"))))

(defclass datagraph2d ()
  ((title
    :initarg :title
    :initform "Graph 2d")
   (xlabel
    :initarg :xlabel
    :initform "x")
   (ylabel
    :initarg :ylabel
    :initform "y")
   (points
    :initarg :points
    :initform '())))

(defmethod datagraph2d-print-points ((graph2d datagraph2d) printer)
  (with-slots (points) graph2d
    (dolist (point points)
      (funcall printer point))))

(defmacro generate-asy-graphing-function (title xlabel ylabel list-of-xvalues list-of-yvalues)
  `(concatenate 'string "plot_" ,title "() {~%"
		"x = {" (array-as-string ,list-of-xvalues) "};~%"
		"y = {" (array-as-string ,list-of-yvalues) "};~%"
		"draw(graph(x,y,Hermite),\"" title "\",mark);~%"
		"}~%"))

(defmethod datagraph2d-plot ((graph2d datagraph2d))
  (with-slots (title xlabel ylabel points) graph2d
    (let ((xvalues (mapcar #'datapoint2d-x points))
	  (yvalues (mapcar #'datapoint2d-y points)))
      (concatenate 'string
		   (generate-asy-graphing-function title xlabel ylabel xvalues yvalues)))))