(in-package :cl-ledger-parsers.test)

(defmethod stream-extended-position ((s row-column-input-stream))
  (list (stream-row s)
        (stream-column s)
        (file-position s)))

(defun ledger-cli-root-directory ()
  (block nil
    (let ((expected-path (asdf:system-relative-pathname :cl-ledger "ledger")))
      (tagbody
       retry
         (let ((src-dir (probe-file expected-path)))
           (unless src-dir
             (cerror "Retry accessing the missing directory."
                     "Directory not found: ~S.
If you are using git please also clone the \"ledger\" submodule." expected-path)
             (go retry))
           (return src-dir))))))

(defun chunk-test (&optional relativep)
  (let* ((ledger-root (ledger-cli-root-directory))
         (root (uiop:pathname-parent-directory-pathname
                (ledger-cli-root-directory)))
         (files (directory
                 (merge-pathnames #P"test/**/*.test" ledger-root))))
    (with-open-file (*standard-output* "/home/chris/ledger.parse"
                                       :direction :output
                                       :if-exists :supersede)
      (time
       (map ()
            (lambda (file &aux printedp)
              (flet ((parse% (buffer)
                       (handler-bind
                           ((space-in-name
                             (lambda (warning)
                               (warn 'space-in-name-with-context
                                     :text buffer
                                     :file file
                                     :inner-error warning)
                               (invoke-restart 'muffle-warning)))
                            (error (lambda (x)
                                     (declare (ignore x))
                                     (return-from parse% nil))))
                         (parse 'cl-ledger-parsers.esrap::chunk
                                buffer))))
                (with-open-file (in file)
                  (map-ledger-chunks-on-shared-buffer 
                   (make-instance 'row-column-input-stream  
                                  :stream in)

                   ;; PRINT CHUNKS THAT CANNOT BE PARSED
                   (lambda (string type position &rest args)
                     (destructuring-bind (row col char) position
                       (declare (ignore col))

                       (case type
                         (:chunk (unless (parse% string)
                                   (unless printedp
                                     (setf printedp t)
                                     (format t "~&#|~%#~90,1,0,'=A~%#|~%"
                                             (format nil "| ~A "
                                                     (if relativep
                                                         (uiop:enough-pathname file root)
                                                         file))))

                                   (format t "~&#~80,1,0,'-<~A~;[Line ~A, Position ~A]~>~%~A"
                                           (format nil "[~S~{ ~S~}]" type args)
                                           row
                                           char
                                           string))))))))))
            files)))))
