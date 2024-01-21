

(define numeric-arithmetic
  (make-arithmetic
    'numeric number? '()

    (lambda (c)
      (case c
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))

    (lambda (opname)
      ;; ugly!!!
      (let ((impl-name (case opname ((neg) '-) ((inv) '/) (else opname))))
        (simple-operation
          opname
          number?
          (implementation-value impl-name))))))




      
          

