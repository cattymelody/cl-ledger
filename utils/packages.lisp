(defpackage #:cl-ledger-utils
  (:use :cl)
  (:export #:scan-match
           #:internal-buffer-overflow
           #:push-extend-with-overflow-check))

(defpackage #:cl-ledger-utils.delegated-stream
  (:use :cl :trivial-gray-streams)
  (:export #:delegated-stream
	   #:lisp-stream))

(defpackage #:cl-ledger-utils.row-column-input-stream
  (:use #:cl
	#:trivial-gray-streams
	#:cl-ledger-utils.delegated-stream)
  (:export #:stream-row
	   #:stream-column
	   #:row-column-input-stream))

