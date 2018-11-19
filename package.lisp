;;;; package.lisp
(defpackage #:pinhole-spell
  (:use #:cl)
  (:export :*dictionary*
	   :add
	   :read-dict
	   :build-misspellings
	   :check
	   :suggest))

(defpackage #:pinhole-data
  (:use #:cl
	#:iter
	#:dbi
	#:sxql
	#:sxql.sql-type)
  (:export :*db*
	   :create-missing-pinhole-tables
	   :pinhole-tables-are-missing))

(defpackage #:pinhole
  (:use #:cl
	#:uiop/os
	#:iter
	#:dbi
	#:sxql
	#:pinhole-spell
	#:pinhole-data)
  (:export :main))
