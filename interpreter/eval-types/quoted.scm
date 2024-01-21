
(define-generic-procedure-handler
  g:eval
  (match-args quoted? environment?)
  (lambda (expr environment)
    (text-of-quotation expr)))


; Syntax

(define (quoted? expr) (tagged-list? 'quote expr))
(define (text-of-quotation quotation) (cadr quotation))

; Constructor

(define (make-quotation expr)
  (list 'quote expr))
