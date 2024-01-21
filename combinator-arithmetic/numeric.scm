
; The number arithmetic

(define numeric-arithmetic
  (make-arithmetic 
    'numeric  number?  '()

    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))

    (lambda (opname)
      (simple-operation 
        opname
        number?
        (get-implementation-value (opname->symbol opname))))))



(define (get-implementation-value symb)
  (case symb
    ((+) n:+)
    ((-) n:-)
    ((*) n:*)
    ((/) n:/)
    ((sin) n:sin)
    ((cos) n:cos)
    ((expt) n:expt)
    ((sqrt) n:sqrt)
    ((magnitude) n:magnitude)))




