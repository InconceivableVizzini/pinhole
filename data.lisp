;;;; data.lisp
(in-package #:pinhole-data)

(defvar *db* nil)

(defun lispify (object)
  (etypecase object
    (symbol (intern (lispify (string-upcase object))
		    (symbol-package object)))
    (string (substitute #\- #\_ object))))

(defun array-convert-nulls-to-nils (results-array)
  (let ((darray (make-array (array-total-size results-array)
			    :displaced-to results-array
			    :element-type (array-element-type results-array))))
    (loop for x across darray
       for i from 0
       do (typecase x
	    ((eql :null)
	     (setf (aref darray i) nil))
	    (cons
	     (setf (aref darray i)
		   (list-convert-nulls-to-nils x)))
	    ((and (not string) vector)
	     (setf (aref darray i)
		   (array-convert-nulls-to-nils x)))))
    results-array))

(defun list-convert-nulls-to-nils (results-list)
  (mapcar (lambda (x)
	    (typecase x
	      ((eql :null)
	       nil)
	      (cons
	       (list-convert-nulls-to-nils x))
	      ((and (not string) vector)
	       (array-convert-nulls-to-nils x))
	      (otherwise
	       x)))
	  results-list))

(defun db-dependent-quoted-identifier (connection)
  (ecase (connection-driver-type connection)
    (:mysql #\`)
    (:postgres #\")
    (:sqlite3 #\")))

;; Quoted identifiers are the escape characters that
;; allow you to enter literal values in database fields.
;; ie `table_name`.
(defmacro with-quoted-identifiers (&body body)
  `(let ((sxql:*quote-character* (or sxql:*quote-character*
				     (db-dependent-quoted-identifier *db*))))
     ,@body))

(defgeneric sql-query (sql &optional binds)
  (:method ((sql string) &optional binds)
    (apply #'dbi:do-sql *db* sql binds))
  (:method ((sql sql-statement) &optional binds)
    (declare (ignore binds))
    (with-quoted-identifiers
	(multiple-value-bind (sql binds)
	    (yield sql)
	  (apply #'dbi:do-sql *db* sql binds)))))

(defgeneric sql-query-results (sql &key binds)
  (:method ((sql string) &key binds)
    (let* ((results
	    (fetch-all
	     (apply #'dbi:execute (prepare *db* sql)
		    binds)))
	   (results
	    (loop for result in results
	       collect
		 (loop for (k v) on result by #'cddr
		    collect (lispify k)
		    collect (cond ((eq v :null) nil)
				  ((and v (listp v))
				   (list-convert-nulls-to-nils v))
				  ((arrayp v)
				   (array-convert-nulls-to-nils v))
				  (t v))))))
      results))
  (:method ((sql sql-statement) &key binds)
    (declare (ignore binds))
    (with-quoted-identifiers
      (multiple-value-bind (sql binds)
	  (yield sql)
	(sql-query-results sql :binds binds)))))

(defun pinhole-tables-are-missing ()
  (eq nil (sql-query-results (select (:name)
			       (from :sqlite_master)
			       (where (:and (:= :type "table")
					    (:= :name "ego")))))))

(defun create-pinhole-tables-if-missing ()
  (if (pinhole-tables-are-missing)
      (sql-query (create-table :ego ((id :type 'integer
					 :primary-key t))))))

;;;; Your mind and personality, including memory,
;;;; knowledge, and skills.
;;;;
;;;; "A character's morph may die, but the character's
;;;; ego may live on, assuming appropriate backup
;;;; measures have been taken. Morphs are expendable,
;;;; but your character's ego represents the ongoing,
;;;; continuous life path of your character's mind and
;;;; personality. This continuity may be interrupted
;;;; by an unexpected death (depending on how recently
;;;; the backup was made), but it represents the
;;;; totality of the character's mental state and
;;;; experiences."
;;(defclass ego ()
  ;; Background
  ;;  
  ;; Factions
  ;; Motivations
  ;; 
;;  )

;;;; "Each background may provide your character
;;;; with certain skills, traits, limitations,
;;;; or other characteristics to start with.
;;;; Keep in mind that your background is where
;;;; you came from, not who you are now. It is
;;;; the past, whereas your faction represents
;;;; whom your character is currently aligned with."
;;(defclass ego-background ()
;; ego-background-modifiers
;; long-term-motivations
;; short-term-motivations
;;)

;;(defclass ego-background-modifier ()
  ;; type (code may loop over modifiers
  ;; looking for specific types, eg during
  ;; player creation)
;;)

;;(defclass background-advantages
;;)

;;(defclass background-disadvantages
;;)

;;(defclass background-common-morphs
;;)

;;;; Your physical body and its capabilities.
;;;;
;;;; "The term morph is used to describe any
;;;; type of form your mind inhabits, whether a
;;;; vat-grown clone sleeve, a synthetic robotic
;;;; shell, a part-bio/part-synthetic 'pod,' or
;;;; even the purely electronic software state
;;;; of an infomorph."
;;(defclass morph ()
;;  t)
