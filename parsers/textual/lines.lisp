(in-package :cl-ledger-parsers.lines)

(defparameter *max-line-size* 65536)

(defun map-lines-on-shared-buffer (stream function &rest arguments) 
  "Call FUNCTION for each line read from STREAM, using an internal buffer.

FUNCTION should accept a single argument, a string which content is
only available while the callback is processing it. The same
underlying buffer is used for successive invocations of FUNCTION.

When FUNCTION signals an error, a CONTINUE restart is active, which
accepts no argument and continue to the next line."
  (check-type stream stream)
  (check-type function (or function symbol))
  (when (symbolp function)
    (setf function (symbol-function function)))
  (let ((buffer (make-array 1024
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (declare (function function))
    (tagbody
     line-loop
       (let ((char (read-char stream nil :eof)))
         (case char
           (#\newline (restart-case
                          (apply function buffer arguments)
                        (continue ()
                          :report "Ignore current line."
                          (go main-loop)))
                      (go main-loop))
           (:eof (apply function buffer arguments)
                 (return-from map-lines-on-shared-buffer (values)))
           (t
            (push-extend-with-overflow-check buffer
                                             char
                                             *max-line-size*)
            (go line-loop))))
     main-loop
       (setf (fill-pointer buffer) 0)
       (go line-loop))))
