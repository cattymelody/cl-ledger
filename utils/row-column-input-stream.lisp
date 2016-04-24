(in-package #:cl-ledger-utils.row-column-input-stream)

(defclass row-column-input-stream (fundamental-character-input-stream
				   delegated-stream)
  ((row :initform 0
	:initarg :initial-row
	:accessor stream-row)
   (column :initform 0
	   :initarg :initial-column
	   :accessor stream-column)))

(defmethod stream-read-char ((stream row-column-input-stream))
  (let ((character (call-next-method)))
    (prog1 character
      (case character
	(#\Newline
	   (incf (stream-row stream))
	   (setf (stream-column stream) 0))
	(t (incf (stream-column stream)))))))

(defmethod print-object ((s row-column-input-stream) output-stream)
  (print-unreadable-object (s output-stream :type t)
    (format output-stream
	    "(~d,~d) : ~s"
	    (stream-row s)
	    (stream-column s)
	    (lisp-stream s))))

