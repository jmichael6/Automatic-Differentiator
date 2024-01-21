

(define (eval-cond expression environment)
  (eval-if (cond->if expression)
           environment))

(define-generic-procedure-handler
  g:eval
  (match-args cond? environment?)
  eval-cond)



; Syntax

(define (cond? expr) (tagged-list 'cond expr))

(define (cond-clauses expr) (cdr expr))

(define (clause-predicate clause) (car clause))

(define (clause-body clause) (sequence->begin (cdr clause)))

(define (cond-else-clause? clause) 
  (tagged-list 'else clause))


; Conversion

(define (cond->if expr)
  (clauses->if (cond-clauses expr)))

(define (clauses->if clauses)
  (if (null? clauses) 
    'the-unspecified-value
    (reduce-clauses (car clauses) (cdr clauses))))

(define (reduce-clauses clause rest-clauses)
  (if (cond-else-clause? clause)
    (if (null? rest-clauses)
      (clause-body clause)
      (error "COND: else clause is not last"))
    (make-if
      (clause-predicate clause)
      (clause-body clause)
      (clauses->if rest-clauses))))



