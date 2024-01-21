

(define (function-extender codomain-arithmetic)
  (make-arithmetic
    'function
    procedure?
    (list codomain-arithmetic)

    (lambda (name codomain-constant)
      codomain-constant)

    (lambda (opname codomain-operation)
      (make-operation
        opname

        (any-arg (operator-arity opname)
                 procedure?
                 (arithmetic-domain-predicate codomain-arithmetic))

        (lambda things
          (lambda args
            (apply  
              (operation-procedure codomain-operation)
              (map (lambda (f) (apply f args))
                   (map (lambda (thing) 
                          (if (procedure? thing) thing (constant-function thing)))
                        things)))))))))


(define (literal-function name)
  (lambda args (cons name args)))

; Helper
(define (constant-function x) (lambda args x))


