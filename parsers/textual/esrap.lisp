(in-package :cl-ledger-parsers.esrap)

;;;;; ESRAP (move to another package)

;;;; TODO REORDER RULES LOGICALLY


(define-condition whitespace-at-boundaries (warning)
  ((name :initarg :name))
  (:report
   (lambda (warning stream)
     (format stream
             "Whitespace at boundaries of account name: ~S"
             (slot-value warning 'name)))))

(defun unspace (args)
  (remove 'whitespace args))

(defun datep (parsed &aux (string (text parsed)))
  (or (ignore-errors
        (periods:strptime string
                          :format "%Y-%m-%d"))
      (periods:strptime string
                        :format "%Y/%m/%d")))

(defrule indent
    (and (+ (or #\space #\tab)))
  (:constant 'whitespace))

(defrule _
    (* (or #\space #\tab))
  (:constant 'whitespace))

(defrule _+
    (+ (or #\space #\tab))
  (:constant 'whitespace))

;; at least two spaces between accounts and currency
(defrule __
    (and #\space (+ #\space))
  (:constant 'whitespace))

(defrule payee
    (+ (not #\newline))
  (:text t))

(defrule flag
    (? (or #\* #\!))
  (:lambda (match)
    (etypecase match
      (string 
       (ecase (char match 0)
         (#\* :cleared)
         (#\! :pending)))
      (null :uncleared))))

(defrule digit
    (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defrule date
    (* (or digit #\/ #\-))
  (:function datep))

(defrule line-comment
    (and _
         (or #\;
             #\|
             #\*
             #\#)
         _
         (* (not #\newline)))
  (:destructure (space1 char space2 content)
                (declare (ignore space1 space2))
                `(comment :character ,char
                          :content ,(text content))))

(defrule posting
    (and indent
         account
         (? (and
             __
             amount))
         _
         (? line-comment))
  (:destructure (indent account amount space comment)
                (declare (ignore indent space))
                (let ((amount (second amount)))
                  (list 'posting
                        :account account
                        :amount amount
                        :comment comment))))

(defrule transaction
    (and _ date _ flag _ payee (+ (and #\newline
                                       (or line-comment
                                           posting))))
  (:function unspace)
  (:destructure (date flag payee newline-and-postings)
                `(transaction ,date ,flag ,payee
                              ,@(mapcar #'second newline-and-postings))))

(defrule account-name
    (+ (not (or #\( #\) #\[ #\] #\: __)))
  (:lambda (match)
    (text match)))

(defrule concrete-account
    (and account-name
         (* (and #\:
                 account-name)))
  (:destructure (name rest)
                (list* :concrete
                       name
                       (mapcar
                        (lambda (data)
                          (let ((name (second data)))
                            (prog1 name
                              (when (ppcre:scan "^\s+|\s+$" name)
                                (warn 'whitespace-at-boundaries name)))))
                               rest))))

(defrule unchecked-virtual-account
    (and #\( concrete-account #\))
  (:destructure (open account close)
                (declare (ignore open close))
                (list* :unchecked-virtual account)))

(defrule checked-virtual-account
    (and #\[ concrete-account #\])
  (:destructure (open account close)
                (declare (ignore open close))
                (list* :checked-virtual account)))

(defrule account
    (or unchecked-virtual-account
        checked-virtual-account
        concrete-account))

(defrule currency
    (or
     (and #\" (* (not #\")) #\")
     (+ (not
         (or digit
             #\-
             #\space
             #\newline))))
  (:text t))

(defrule space #\Space (:constant :space))

(defrule basic-amount
    (or prefixed-amount
        postfixed-amount
        unitless-amount))

(defrule prefixed-amount
    (and currency (? space) amount-quantity)
  (:destructure (currency spacep quantity)
                `(amount :quantity ,quantity
                         :prefixed t
                         :connectedp ,(not spacep)
                         :currency ,currency)))

(defrule postfixed-amount
    (and amount-quantity (? space) currency)
  (:destructure (quantity spacep currency)
                `(amount :quantity ,quantity
                         :prefixed nil
                         :connectedp ,(not spacep)
                         :currency ,currency)))

(defrule unitless-amount
    amount-quantity
  (:lambda (q)
    `(amount :quantity ,q
             :prefixed nil
             :connectedp nil
             :currency nil)))

(defrule amount
    (and
     basic-amount
     annotations
     (? assertion)))

(defrule assertion
    (and _ #\= _ basic-amount)
  (:destructure (white equal space amount)
                (declare (ignore white space equal))
                amount))

(defrule annotations
    (* annotation))

(defrule annotation
    (and _ (or amount-price
               amount-cost
               amount-note))
  (:function second))

(defrule amount-cost
    (and
     #\@ (? #\@) _ basic-amount)
  (:destructure (at totalp space basic)
                (declare (ignore at space))
                `(:cost :totalp ,(and totalp t)
                        :amount ,basic)))

(defrule amount-note
    (and #\( (* (not #\))) #\))
  (:destructure (L M R)
                (declare (ignore L R))
                (list :note 
                      (text M))))


(defrule amount-price
    (or amount-total-price amount-lot-price)
  ;; TODO: NOT SURE ABOUT HOW TO HANDLE FIXATED PRICES
  )

(defrule amount-total-price
    (and #\{ amount-lot-price #\})
  (:destructure (open (lot &key totalp value) close)
                (declare (ignore totalp open lot close))
               (list :price :totalp t :value value)))

(defrule amount-lot-price
    (and #\{ amount-value #\})
  (:destructure (open VALUE close)
                (declare (ignore open close))
                (list :price :totalp nil :value VALUE)))

(defrule amount-value
    (and (? #\=)
         (+ (not (or #\{ #\}))))
  (:destructure (fixatedp raw)
                (let ((amount (parse 'basic-amount (text raw))))
                  (if fixatedp
                      `(:fixated ,amount)
                      amount))))

;; (parse 'transaction 
;;              "    2011-01-01 * Opening balance
;;     Assets:Bank                    10.00 GBP
;;     Equity:Opening balance")

;; (parse 'transaction 
;;        "2012-03-09 KFC
;;     Expenses:Food                10 CHIK @ $50
;;     Assets:Cash")

;; (parse 'amount-price
;;        "{=$50}")

;; (cambl:parse-amount "10 CHIK @ $50")

(defrule amount-quantity
    (or negative-quantity
        positive-quantity))

(defun interpret-natural-quantity (integer fractional)
  (let ((integer (parse-integer integer)))
    (if fractional
        (destructuring-bind (separator value) fractional
          (if value
              `(quantity :separator ,separator
                         :value ,(+ integer
                                    (/ (parse-integer value)
                                       (expt 10 (length value))))
                         :precision ,(length value))
              `(quantity :separator ,separator
                         :value ,integer
                         :precision 0)))
        `(quantity :separator nil
                   :value ,integer
                   :precision 0))))

(defrule natural-with-thoushand-marks
    (and digit
         (? digit)
         (? digit)
         (+ (and #\, digit digit digit)))
  (:text t)
  (:lambda (s) (ppcre:regex-replace-all "," s "")))

(defrule positive-quantity
    (or thousand-mark-quantity
        unmarked-quantity))

(defrule thousand-mark-quantity
    (and natural-with-thoushand-marks
         (? (and #\. (? natural))))
  (:destructure (integer fractional)
                (append 
                 (interpret-natural-quantity integer
                                             fractional)
                 (list :thousand-marks-p t))))

(defrule unmarked-quantity
    (and natural
         (? (and decimal-separator             
                 (? natural))))
  (:destructure (integer fractional)
                (append (interpret-natural-quantity integer fractional)
                        (list :thousand-marks-p nil))))

(defrule negative-quantity
    (and #\- positive-quantity)
  (:destructure (minus (keyword &key thousand-marks-p separator value precision))
                (declare (ignore minus))
                (list keyword
                      :separator separator
                      :value (- value)
                      :precision precision
                      :thousand-marks-p thousand-marks-p)))

(defrule decimal-separator
    (or #\, #\.)
  (:lambda (s)
    (ecase (char s 0)
      (#\, :comma)
      (#\. :dot))))

(defrule natural
    (+ (character-ranges (#\0 #\9)))
  (:text t))

(defrule chunk transaction)

