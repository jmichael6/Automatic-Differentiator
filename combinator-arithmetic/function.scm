

(define (pure-function-extender codomain-arithmetic)
  (make-arithmetic
    'pure-function 
    (disjoin function? (arithmetic-domainp codomain-arithmetic)) ; the objects can be both functions or objects of codomain arithmetic
    (list codomain-arithmetic)

    (lambda (name codomain-constant)
      codomain-constant)

    (lambda (opname codomain-operation)
      (make-operation
        opname
        (any-arg (operator-arity opname)
                 procedure?
                 (arithmetic-domainp codomain-arithmetic))
        (lambda things  
          (lambda args
            (apply-operation codomain-operation
                             (map (lambda (thing)
                                    (if (function? thing)  ; Here we handle take non-functions (objects of codomain arithmetic) 
                                      (apply thing args)
                                      thing))
                                  things))))))))


; Literal functions

(define (literal-function name)
  (lambda args
    (cons name args)))


; Helpers

(define (function? f) (procedure? f))
