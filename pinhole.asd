;;;; pinhole.asd

(asdf:defsystem #:pinhole
  :description "Describe pinhole here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "pinhole")
	       (:file "spelling")
	       (:file "data"))
  :depends-on (#:iterate
               #:dbi
               #:sxql)
  :build-operation "asdf:program-op"
  :build-pathname "pinhole"
  :entry-point "pinhole:main")
