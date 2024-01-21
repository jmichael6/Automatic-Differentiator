
; Applications - Will be the default

(define (eval-application application environment)
  (g:apply (g:advance 
             (g:eval (application-operator application)))
           (application-operands application) ; we pass the unevaluated operands to apply
           environment))


; Application - syntax

(define (application? expr) (pair? expr))
(define (application-operator app) (car app))
(define (application-operands app) (cdr app))


; Constructor

(define (make-application operator operands) 
  (cons operator operands))
