(in-package :cl-ledger-utils)

(defmacro scan-match (expr &body clauses)
  "A case expression based on regular expressions.

Each clause is a list made of an unevaluated regular expression (or T)
followed by a body. The regular expression is any expression
understood by CL-PPCRE, i.e. either a string or a parse tree. 

A clause whose head is T always matches and does not actually scan the
input expression EXPR. Each expression is tried in sequence as soon as
one matches, in which case its associated body is executed. 

Like COND, the returned value is NIL if no pattern matches the input
expression. 

Clauses are compiled at load-time."
  (let ((line (gensym)))
    `(let ((,line ,expr))
       (cond
         ,@(loop
              for (regex . body) in clauses
              collect
                (case regex
                  ((t) `(t ,@body))
                  (otherwise
                   `((ppcre:scan 
                      (load-time-value
                       (let ((ppcre:*use-bmh-matchers* t))
                         (ppcre:create-scanner ',regex)))
                      ,line)
                     ,@body))))))))

(define-condition internal-buffer-overflow (error)
  ((variable :initarg :variable)
   (size     :initarg :size))
  (:report (lambda (condition stream)
             (with-slots (variable size) condition
               (format stream
                       "Internal parse buffer is getting large.~% ~
                        Current size is ~D.~% ~
                        To avoid error messages, increase ~S."
                       size
                       variable)))))

(defmacro push-extend-with-overflow-check (buffer% element variable)
  (let ((size (gensym))
        (buffer (gensym)))
    `(let* ((,buffer ,buffer%)
            (,size (array-total-size ,buffer)))
       (when (>= ,size ,variable)
         (cerror "CONTINUE"
                 'internal-buffer-overflow
                 :variable ',variable
                 :size ,size))
       (vector-push-extend
        ,element
        ,buffer
        ,size))))
