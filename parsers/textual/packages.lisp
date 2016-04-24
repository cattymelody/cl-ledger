;; CLEAN-UP PACKAGES, UTILS,...

(defpackage :cl-ledger-parsers.lines
  (:use :cl)
  (:export #:map-lines-on-shared-buffer)
  (:import-from  #:cl-ledger-utils
                 #:push-extend-with-overflow-check))

(defpackage :cl-ledger-parsers.chunks
  (:use #:cl
        #:cl-ppcre
        #:cl-ledger-parsers.lines)
  
  (:import-from #:cl-ledger-utils
                #:scan-match
                #:push-extend-with-overflow-check)
  (:export
   
   #:*chunk-buffer-initial-size*
   #:*chunk-buffer-maximum-size*

   #:chunk-parse-error
   #:incomplete-chunk
   #:unexpected-block-end
   #:unexpected-whitespace
   
   #:map-ledger-chunks-on-shared-buffer
   #:scan-match

   #:stream-extended-position
   #:with-errors-as-garbage))

(defpackage :cl-ledger-parsers.esrap
  (:use #:cl
        #:esrap
        #:cl-ledger-parsers.chunks)
  (:export #:chunk
           #:transaction))

(defpackage :cl-ledger-parsers.test
  (:use #:cl
        #:esrap
        #:cl-ledger-utils.row-column-input-stream
        #:cl-ledger-parsers.chunks
        #:cl-ledger-parsers.esrap))
