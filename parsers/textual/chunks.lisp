(in-package :cl-ledger-parsers.chunks)

;;;;
;;;; CONFIGURABLE ELEMENTS
;;;;

(defvar *chunk-buffer-initial-size* 4096
  "Initial size of the chunk buffer.")

(defvar *chunk-buffer-maximum-size*
  (* (expt 2 12)
     *chunk-buffer-initial-size*)
  "Size at which errors are raised when growing internal buffer.")

(defgeneric stream-extended-position (stream)
  (:documentation
   "Calls STREAM-FILE-POSITION by default, but can be specialised to return more.")
  (:method (s) (file-position s)))

;;;;
;;;; ERRORS
;;;;

(define-condition chunk-parse-error (error)
  ((stream :initarg :stream)
   (position :initarg :position))
  (:documentation "Base class for all chunk-related parse error."))

(define-condition incomplete-chunk (chunk-parse-error)
  ((state :initarg state))
  (:documentation "A chunk near end-of-file was parsed incompletely."))

(define-condition unexpected-whitespace (chunk-parse-error) ()
  (:documentation "Unexpected whitespace at beginning of line."))

(define-condition unexpected-block-end (chunk-parse-error) ()
  (:documentation "Unexpected 'end' keyword matching no 'comment' or 'test' block."))

;;;;
;;;; MAP OVER LEDGER CHUNKS
;;;;

(defun map-ledger-chunks-on-shared-buffer (stream function)
  "Call FUNCTION for each Ledger chunk read from STREAM.

FUNCTION should be a function of type (STRING SYMBOL &REST T). More
precisely, it should be ready to accept:

  - A STRING. Since we don't want to allocate memory when not
    required, the string corresponds to the current state of the
    internal buffer and is as such only meaningful for the duration of
    the callback function. The content should be copied explicitely in
    order to be stored elsewhere (e.g. COPY-SEQ, SUBSEQ, ...). For
    example, do not use the :SHAREDP option in CL-PPCRE since the
    content of the buffer will change over time.

  - A keyword argument TYPE specifying the kind of chunk that was
    parsed (see below).

  - Zero or more arguments associated with the current value of TYPE.

The following describes the possible configurations of TYPE and the
corresponding additional arguments that are emitted by the parser.
Below, COMMAND-LINE-ARGUMENTS represents a copy of the string that
follows the 'test' keyword in the file.

    :TEST :ERROR COMMAND-LINE-ARGUMENTS
    Test with error message only.

    :TEST :OUTPUT COMMAND-LINE-ARGUMENTS
    Test with normal output only.

    :TEST :MIXED COMMAND-LINE-ARGUMENTS END-OUTPUT START-ERROR 
    Test with mixed output/error (error typically contains warning
    messages). The standard ouput is (SUBSEQ STRING 0 END-OUTPUT),
    whereas the error output is (SUBSEQ STRING START-ERROR).

    :GARBAGE ERROR 
    Invalid entry due to the error-object ERROR, which is a non-NIL
    instance of CHUNK-PARSE-ERROR.  A garbage entry is built only
    through the invocation of the GARBAGE restart (see below).

    :PYTHON 
    String contains Python statements.

    :COMMENT
    Block comments.

    :LINE-COMMENTS
    Consecutive line comments, where the comment characters at the
    beginning of each line are left.

    :CHUNK 
    A directive or a transaction. In fact, anything that is not
    categorized as one of the above types.

Subtypes of CHUNK-PARSE-ERROR might be signalled during processing. If
this is the case, two restarts are available, which takes no argument:

  - SKIP: ignore current text and try to advance to the next
    chunk. Generally this means that the parser will look for a
    non-empty, non-indented line from where to start parsing
    again.

  - GARBAGE: Read lines up-to the beginning of the next chunk and
    store the invalid text as a :GARBAGE entry, where the additional
    argument is the error object reported by the function. Garbage
    entries are useful in particular with files which, for the purpose
    of a test, contains invalid entries, followed by a :TEST section.

Additionally, since we rely on MAP-LINES-ON-SHARED-BUFFER, it is
possible to skip a single line.

In case one of the internal LINE buffer or CHUNK buffer get
exceptionally large, the function reports a continuable
CL-LEDGER-UTILS:INTERNAL-BUFFER-OVERFLOW error. See code for details.
"
  (check-type function function)
  (let (;; Our parser is a state machine. The STATE variable holds the
        ;; function currently handling incoming lines of inputs. It is
        ;; initialized here to a void lambda form (instead of NIL) so
        ;; that we can declare its type here below to be FUNCTION
        ;; (chicken-and-egg problem between LET and LABELS: can't set
        ;; it to #'DEFAULT here).
        (state (lambda ()))

        ;; Position inside stream at beginning of chunk
        start-position
        
        ;; Relative position inside chunk
        (relative-position 0)

        ;; Current error (for :GARBAGE entries)
        current-error

        ;; (keyword . regex) cons-cell for matching the end of a block
        block-terminator

        ;; Command-line and arguments associated with a test block
        test-command
        
        ;; Internal buffer representing current chunk's content. The
        ;; element-type of the buffer is the same as the stream one. I
        ;; only tested it with CHARACTER, though.
        (buffer (make-array
                 *chunk-buffer-initial-size*
                 :element-type (stream-element-type stream)
                 :adjustable t
                 :fill-pointer 0)))
    (declare (fixnum relative-position)
             ((integer 0) relative-position)
             (function state))
    (labels
        ((reset ()
           ;; Initialize this parser's state
           (setf current-error         nil
                 block-terminator      nil
                 test-command          nil
                 start-position        nil
                 relative-position     0
                 (fill-pointer buffer) 0
                 state                 #'default))

         (add (character)
           ;; Add CHARACTER into internal buffer, extending it if
           ;; necessary. There is a maximal buffer size over which a
           ;; continuable error is reported, in order to provide a
           ;; regular behavior across before hitting implementation-
           ;; defined handling of memory over-allocation.  Also, set
           ;; START-POSITION if this is the first time (since reset)
           ;; that we add a line.
           (unless start-position
             (setf start-position
                   (stream-extended-position stream)))
           (incf relative-position)
           (push-extend-with-overflow-check buffer
                                            character
                                            *chunk-buffer-maximum-size*))

         (fail (error-name &optional line)
           ;; Accumulate LINE into buffer and report an error.
           (when line (buffer line))
           (error (setf current-error
                        (make-condition error-name
                                        :stream stream
                                        :position relative-position))))

         (buffer (line)
           ;; Accumulate LINE into internal buffer and add a newline
           ;; at the end.
           (map () #'add line)
           (add #\newline))

         (emit (type &rest args)
           ;; Call user function with current buffer's content and
           ;; reset the state, a keyword representing the type of the
           ;; chunk that was parsed as well as additional data
           ;; meaningful for that type.
           
           (when (plusp (fill-pointer buffer))
             ;; Since we always add a newline in the #'BUFFER function
             ;; (which is the only way we add lines in the internal
             ;; buffer), we know that the last character in our
             ;; buffer, if it exists, is a newline. That last newline
             ;; is discarded here, by decrementing the
             ;; fill-pointer. The rationale is that the newline is
             ;; generally useless and that the callback function has
             ;; more control over the way the text might be
             ;; formatted. It is easier to (i) remove the newline here
             ;; and (ii) add a newline when necessary later in a
             ;; pretty-printer than leaving the newline here.
             (decf (fill-pointer buffer)))
           
           (apply function
                  buffer
                  type
                  (or start-position (stream-extended-position stream)) 
                  args)
           (reset))

         (start-delimited-block (line type)
           ;; Initialize the start of delimited blocks (comments,
           ;; tests, python, maybe others...)
           (ecase type
             (:test
              ;;  tests are made of a command and arguments. We store
              ;;  it temporarily in TEST-COMMAND.
              (setf test-command
                    (subseq line #.(length "test "))))
             ;; Exhaustivity check
             ((:comment :python)))

           ;; Setup the regular expression that should be used to
           ;; detect the end of our block. For tests and comments, we
           ;; look for the corresponding "end test", "end comment"
           ;; lines. For Python blocks, we search for the start of new
           ;; chunks, i.e. lines starting with a non-space character
           ;; at first position.
           ;;
           ;; The C++ version currently allows "comment" to end with
           ;; "end test" (and inversely) but I consider this to be a
           ;; latent bug.
           (setf block-terminator
                 (assoc type
                        (load-time-value
                         (let ((ppcre:*use-bmh-matchers* t))
                           (list*
                            (cons :python
                                  (ppcre:create-scanner
                                   '(:sequence
                                     :start-anchor
                                     :non-whitespace-char-class)))
                            (loop
                               for block in '(:test :comment)
                               collect
                                 (cons block
                                       (ppcre:create-scanner
                                        `(:sequence
                                          :start-anchor
                                          "end"
                                          (:greedy-repetition 1 NIL :whitespace-char-class)
                                          ,(string-downcase (string block))
                                          (:greedy-repetition 0 NIL :whitespace-char-class)
                                          :end-anchor)))))))))
           (setf state #'delimited-block))

         ;;=========================
         ;;
         ;; STATE MACHINE FUNCTIONS
         ;;
         ;;=========================
         
         (skip-and-ignore (line)
           ;; Skip input until the start of the next chunk, discarding
           ;; intermediate lines.
           (scan-match line
             ("^\\S" (setf state #'default)
                     (default line))))

         (skip-and-garbage (line)
           ;; Skip input until the start of the next chunk, and emit
           ;; intermediate lines as garbage.
           (scan-match line
             ("^\\S" (emit :garbage current-error)
                     (setf state #'default)
                     (default line))
             (t (buffer line))))

         (line-comments (line)
           ;; Read consecutive line comments until a non-comment line
           ;; appears. In particular, empty lines separate groups of
           ;; comments. Global comments are generally not tied to
           ;; transactions or directives in Ledger.
           (scan-match line
             ("^[;#*|]" (buffer line))
             (t
              (emit :line-comments)
              (setf state #'default)
              (default line))))
         
         (delimited-block (line)
           ;; Reading begin/end blocks like comments, tests, python...
           (destructuring-bind (keyword . scanner) block-terminator
             (if (ppcre:scan scanner line)
                 (case keyword
                   ;; test and comment have "end" delimiters.  The
                   ;; line containing "end" is not kept, just the
                   ;; content.
                   (:comment (emit :comment))
                   
                   ;; For tests, we also report the previously parsed
                   ;; test command and arguments.
                   (:test (multiple-value-bind (begin end)
                              (ppcre:scan "__ERROR__" buffer)
                            (step
                             (cond
                               ((not begin)
                                (emit :test :output test-command))
                               ((zerop begin)
                                (replace buffer buffer :start1 0 :start2 (1+ end))
                                (decf (fill-pointer buffer) #.(length "__ERROR__"))
                                (emit :test :error test-command))
                               (t
                                (emit :test :mixed test-command begin (1+ end)))))))
                   
                   ;; Python blocks terminates when other chunks
                   ;; start.  That's why we process the directly
                   ;; process LINE in the default mode.
                   (:python (emit :python)
                            (default line)))
                 (buffer line))))

         (indented (line)
           ;; Read consecutive indented lines.
           (scan-match line
             ("^\\s"
              ;; current line is indented: accumulate into buffer.
              (buffer line))
             
             (t
              ;; non-indented line: terminate default chunk...
              (emit :chunk)
              ;; ... and directly continue with (DEFAULT LINE) since
              ;; this line does not belong to the previous chunk.
              (default line))))

         (default (line)
           ;; Dispatch to an appropriate state w.r.t. input line.
           (scan-match line
             ("^[;#*|]"                (buffer line)
                                       (setf state #'line-comments))
             ("^python"                (start-delimited-block line :python))
             ("^comment"               (start-delimited-block line :comment))
             ("^test"                  (start-delimited-block line :test))
             ("^end\\s+(comment|test)" (fail 'unexpected-block-end line))

             ;; empty line
             ("^\\s*$"                 (return-from default))

             ;; indent in non-indent context
             ("^\\s"                   (fail 'unexpected-whitespace line))

             ;; start of a directive/transaction
             (t                        (buffer line)
                                       (setf state #'indented)))))
      (reset)
      (map-lines-on-shared-buffer stream
                  (lambda (line &aux normal-exit-p)
                    (unwind-protect
                         (restart-case 
                             (progn
                               (funcall state line)
                               (setf normal-exit-p t))
                           (skip ()
                             :report "Skip to the next chunk."
                             (setf normal-exit-p t)
                             (setf state #'skip-and-ignore))
                           (garbage ()
                             :report "Wrap bad input up-to next chunk as a (:GARBAGE ...) entry."
                             (setf normal-exit-p t)
                             (setf state #'skip-and-garbage)))
                      (unless normal-exit-p
                        ;; NORMAL-EXIT-P is set to T after normal
                        ;; termination of the callback function, or
                        ;; when we invoke restarts over which we have
                        ;; control. Otherwise, the state machine is
                        ;; reset. This is useful in particular in
                        ;; combination with the restart established by
                        ;; MAP-LINES-ON-SHARED-BUFFER, which allows to
                        ;; skip a single line. In that case, calling
                        ;; (RESET) allows to empty the buffer and
                        ;; retry with the default state.
                        (reset)))))
      (when (plusp (fill-pointer buffer))
        (restart-case
            (fail 'incomplete-chunk)
          (skip ()
            :report "Ignore incomplete chunk.")
          (garbage ()
            :report "Return incomplete chunk as a (:GARBAGE ...) entry."
            (emit :garbage current-error))))
      (values))))

(defmacro with-errors-as-garbage (&body body)
  "Execute BODY and invoke the GARBAGE restart for each CHUNK-PARSE-ERROR."
  `(handler-bind
       ((chunk-parse-error
         (lambda (cpe)
           (declare (ignore cpe))
           (invoke-restart 'garbage))))
     ,@body))

