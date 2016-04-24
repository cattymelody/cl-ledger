(in-package :cl-ledger-parsers.test)

(parse 'transaction
       "  2012-03-09 KFC
    Assets:Cash                  $75
    Expenses:Food                -10 CHIK {$50} @ $75
    Equity:Capital Gains         $-25")

(parse 'transaction
       "2012/01/01 * KFC
    # CSV: 2012/01/01,KFC,$10
    ; Imported: 2014/08/01
    ; UUID: 4352cc5a03f882f6f159b90a518667bde7200351
    Expenses:Unknown                             $10
    Equity:Unknown")

(parse 'chunk
       "2009/02/03 Entry
    Expenses:Cash                 $100.00
    (Assets:Cash )                  $-100.00 = $9,700.00")


(with-errors-as-garbage 
  (chunk-test))

