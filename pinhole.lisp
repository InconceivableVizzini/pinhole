;;;; pinhole.lisp
(in-package #:pinhole)

(defparameter *running* nil)

(defmacro string-is-one-of (s set)
  `(member ,s ,set :test #'string-equal))

(defmacro string-is (s comparison-string)
  `(string-equal ,s ,comparison-string))

(defun prompt (&optional (message ">"))
  (clear-input)
  (write-string message)
  (finish-output)
  (read-line))

(defun crlf-prompt ()
  (if (os-windows-p) (write-char #\return))
  (write-char #\linefeed)
  (finish-output))

(defun clear-prompt ()
  (format t "~A[H~@*~A[J" #\escape)
  (finish-output))

(defun recommend-player-input ()
  (write-string "exit, quit, clear")
  (crlf-prompt))

(defun start-running-game ()
  (setq *running* t))

(defun stop-running-game ()
  (setq *running* nil))

(defun init-game ()
  (create-pinhole-tables-if-missing)
;;  (load-game-objects)
;;  (prepare-spelling-suggestions)
  (start-running-game))

(defun action-possibly-misspelled (input)
  (let ((suggestions (suggest input)))
    (format t "Did you mean one of the following?~%")
    (format t "~{~a~^, ~}" suggestions)
    nil))

(defun step-game ()
  (let ((input (prompt)))
    (cond
      ((string-is-one-of input '("exit"
				 "quit"))
       (stop-running-game))
      ((string-is input "clear") (clear-prompt))
      ((string-is input "") (recommend-player-input))
      (t (format t "going to keep running~%")))))

(defun deinit-game ()
  t)

(defun main ()
  (with-connection (db :sqlite3 :database-name "pinhole.db")
    (setq *db* db)
    (init-game)
    (loop :while *running* :do
       (step-game))
    (deinit-game)))
