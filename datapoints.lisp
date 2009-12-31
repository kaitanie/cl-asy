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