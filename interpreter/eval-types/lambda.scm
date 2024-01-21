
(define (eval-lambda expr environment)
  (make-compound-procedure 
    (lambda-parameters expr)
    (lambda-body expr)
    environment))

(define-generic-procedure-handler
  g:eval
  (match-args lambda? environment?)
  eval-lambda)



; Selectors

(define (lambda? expr) (tagged-list 'lambda expr))

(define (lambda-parameters expr) (cadr expr))

(define (lambda-body expr) 
  (sequence->begin (cddr expr)))


; Constructor

(define (make-lambda parameters body) ; body = begin form | single expression (not a sequence of expressions)
  (cons 'lambda
        (cons parameters 
              (if (begin? body) 
                (begin-statements body)
                (list body)))))

