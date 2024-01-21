
(define (eval-begin expr environment)
  (eval-sequence (begin-statements expr) environment))

(define (eval-sequence statements environment)
  (cond ((null? statements) (error "EVAL-SEQUENCE: statements is null"))
        ((null? (cdr statements)) (g:eval (car statements) environment))
        (else 
          (g:eval (car statements) environment)
          (eval-sequence (cdr statements) environment))))

(define-generic-procedure-handler
  g:eval
  (match-args begin? environment?)
  eval-begin)
  


; Syntax


(define (begin? expr) (tagged-list? 'begin expr))

(define (begin-statements expr) (cdr expr))

(define (make-begin statements) (cons 'begin statements))


; Constructors

(define (sequence->begin seq) 
  (if (null? seq) 
    'the-unspecified-value
    (if (null? (cdr seq))
      (car seq)
      (make-begin
        (flatmap 
          (lambda (statement)
            (if (begin? statement)
              (begin-statements statement)
              (list statement)))
          seq)))))


