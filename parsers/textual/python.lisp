(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "DON'T LOAD THIS FILE"))

(defun dedent-python-block (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
         with indent
         for line = (read-line in nil :eof)
         until (eq :eof line)
         do
           (format out "~&~A" (if (> (length line) 4)
                                  (subseq line 4)
                                  line))))))

(defun parse-python (string)
  (format nil "~S"
          (clpython:parse (dedent-python-block string))))
