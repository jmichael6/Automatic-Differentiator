

(define (symbolic-extender base-arithmetic)
  (make-arithmetic
    'symbolic symbol? (list base-arithmetic)

    (lambda (name base-constant)
      base-constant)

    (lambda (opname base-operation)
      (make-operation
        opname
        (any-arg (operator-arity opname)
                 non-literal
                 (arithmetic-domainp base-arithmetic))
        (lambda args
          (cons opname args))))))


(define (combined-arithmetic arithmetic)
  (extend-arithmetic symbolic-extender arithmetic))

; Helpers
                 
(define (non-literal x) (or (symbol? x) (pair? x)))


