(defsystem #:cl-ledger-utils
  :serial t
  :depends-on (#:trivial-gray-streams)
  :components ((:file "packages")
               (:file "utils")
               (:file "delegated-stream")
               (:file "row-column-input-stream")))

