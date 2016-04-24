(in-package #:cl-ledger-utils.delegated-stream)

(defclass delegated-stream (fundamental-stream)
  ((lisp-stream :documentation "Accessor for inner standard stream."
		:accessor lisp-stream
		:initarg :stream))
  (:documentation "Gray stream delegating to standard stream.

DELEGATED-STREAM provides a fundamental Gray stream mixin which
delegates all stream methods (e.g. STREAM-WRITE-CHAR) to its
underlying standard stream LISP-STREAM (e.g. WRITE-CHAR).

Example:

  ;; Define an output stream which tracks current line number.
  ;; Here the FUNDAMENTAL-CHARACTER-OUTPUT-STREAM superclass
  ;; provide specialized methods, like for example STREAM-TERPRI,
  ;; which are implemented in terms of other stream function
  ;; (e.g. STREAM-WRITE-CHAR). DELEGATED-STREAM's methods are
  ;; only used when no more specific behavior is available, which
  ;; is why the class is the least important superclass.

  (defclass line-counting-stream
      (fundamental-character-output-stream 
       delegated-stream)
    ((line :initform 1 :accessor stream-line)))


  ;; Increment internal counter when writing a newline.

  (defmethod stream-write-char ((s line-counting-stream) char)
    (prog1 (call-next-method)
      (when (char= #\Newline char)
        (incf (stream-line s)))))


  ;; Test

  (with-open-file (stream #P\"/tmp/test\"
   			  :direction :output)
    (let ((s (make-instance 'line-counting-stream 
   			    :stream stream)))
      (loop
         repeat 50
         do (format s \"~2,'0d ~16,'0x~%\"
  		  (stream-line s)
  		  (random most-positive-fixnum)))))

   ;;  --- /tmp/test ----

   01 10C6360BD14AC42F
   02 3B5D0D66B8F1CD0D
   03 3468C613265D94FD
   04 31F28F03A8DBE3DD
   05 2C16811B84C2A353
   ...
"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun docstring (fname &optional details)
    "Produce a docstring about a method delegating its computation to
the inner LISP-STREAM stream. The optional DETAILS argument is
responsible to add, if necessary, a final period."
    (format nil "Calls CL:~A on (LISP-STREAM STREAM)~:[.~; ~:*~A~]" fname details)))

;;; Delegation where Gray methods and standard stream functions share
;;; a compatible argument list.

(macrolet ((delegate (gray-method)
	     (let ((lisp-function
		    (find-symbol (subseq (string gray-method)
					 #.(length "stream-"))
				 :common-lisp)))
	       `(defmethod ,gray-method ((stream delegated-stream))
		  ,(docstring lisp-function)
		  (,lisp-function (lisp-stream stream))))))
  (delegate stream-clear-input)
  (delegate stream-file-position)
  (delegate stream-force-output)
  (delegate stream-read-line)
  (delegate stream-clear-output) 	
  (delegate stream-finish-output) 	
  (delegate stream-fresh-line) 	
  (delegate stream-listen) 	
  (delegate stream-read-byte) 	
  (delegate stream-read-char-no-hang) 	
  (delegate stream-terpri))

;;; Special methods that cannot be delegated as above

(defmethod stream-element-type ((stream delegated-stream))
  #.(docstring 'stream-element-type)
  (stream-element-type (lisp-stream stream)))

(defmethod stream-read-char ((stream delegated-stream))
  #.(docstring 'stream-read-char)
  (read-char (lisp-stream stream) nil :eof))

;;; Delegation for reversed argument lists (e.g. WRITE-CHAR and
;;; STREAM-WRITE-CHAR takes arguments in opposite order).

(defmethod stream-write-char ((stream delegated-stream) character)
  #.(docstring 'write-char)
  (write-char character (lisp-stream stream)))

(defmethod stream-write-byte ((stream delegated-stream) byte)
  #.(docstring 'write-byte)
  (write-byte byte (lisp-stream stream)))

(defmethod stream-unread-char ((stream delegated-stream) character)
  #.(docstring 'unread-char)
  (unread-char character (lisp-stream stream)))

;;; Normal vs. keyword arguments

(defmethod stream-read-sequence
    ((stream delegated-stream) sequence start end &key &allow-other-keys)
  #.(docstring 'read-sequence)
  (read-sequence (lisp-stream stream) sequence :start start :end end))

(defmethod stream-write-sequence
    ((stream delegated-stream) sequence start end &key &allow-other-keys)
  #.(docstring 'write-sequence)
  (write-sequence (lisp-stream stream) sequence :start start :end end))

(defmethod stream-write-string
    ((stream delegated-stream) string &optional start end)
  #.(docstring 'write-string)
  (write-string string (lisp-stream stream) :start start :end end))

;; PEEK-CHAR

(defmethod stream-peek-char ((stream delegated-stream))
  #.(docstring 'peek-char
	       "with a PEEK-TYPE of NIL as explained in 
STREAM-PEEK-CHAR's documentation.")
  (peek-char nil (lisp-stream stream)))
