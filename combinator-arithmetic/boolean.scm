
(define boolean-arithmetic
  (make-arithmetic
    'boolean boolean? '()

    (lambda (name)
      (case name
        ((additive-identity) #f)
        ((mutiplicative-identity) #t)
        (else (default-object))))

    (lambda (opname)
      (simple-operation
        opname
        boolean?
        (boolean-dispatch opname)))))

(define (boolean-dispatch opname) 
  (case opname
    ((+) (lambda (a b) (or a b)))
    ((*) (lambda (a b) (and a b)))
    ((-) (lambda (a b) (and a (not b))))
    ((neg) not)
    (else (error "Invalid operation used on booleans"))))
