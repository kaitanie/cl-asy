(defsystem :cl-asy
  :name :cl-asy
  :version "0.1"
  :author "Pekka Kaitaniemi <pekka.kaitaniemi@gmail.com>"
  :license "BSD"
  :serial t
  :components ((:file "cl-asy")
	       (:file "histo")
	       (:file "datapoints")
	       (:file "plot"))
;;  )
  :depends-on (:alexandria))
