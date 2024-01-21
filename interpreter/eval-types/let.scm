

(define (eval-let expr environment)
  (g:eval (let->application expr)
          environment))

(define (let->application expr) 
  (make-application
    (make-lambda 
      (map binding-parameter (let-bindings expr))
      (let-body expr))
    (map binding-value (let-bindings expr))))

(define-generic-procedure-handler
  g:eval
  (match-args let? environment?)
  eval-let)


; Selectors

(define (let? expr) (tagged-list? 'let expr))

(define (let-bindings expr) (cadr expr))

(define (binding-parameter binding) (car binding))

(define (binding-value binding) (cadr binding))

(define (let-body expr) 
  (sequence->begin (cddr expr)))


