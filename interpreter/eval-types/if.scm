
(define (eval-if expression environment)
  (if (g:advance
        (g:eval (if-predicate expression) environment))
    (g:eval (if-consequent expression) environment)
    (g:eval (if-alternative expression) environment)))

(define-generic-procedure-handler
  g:eval
  (match-args if? environment?)
  eval-if)

; Syntax

(define (if? expr) (tagged-list 'if expr))

(define (if-predicate expr) (cadr expr))

(define (if-consequent expr) (caddr expr))

(define (if-alternative expr)
  (if (null? (cdddr expr))
    'the-unspecified-value
    (cadddr expr)))

; Constructor


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

