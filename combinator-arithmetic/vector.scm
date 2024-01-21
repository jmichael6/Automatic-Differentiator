
(define (vector-extender base-arithmetic)
  (make-arithmetic
    'vector vector? (list base-arithmetic)

    (lambda (name base-constant)
      base-constant)

    (lambda (opname base-operation)
      (case opname
        ((+ - neg) (vector-additive-operation opname base-arithmetic))
        ((*) (operation-union '* (dot-operation base-arithmetic) (scale-operation base-arithmetic)))
        ((magnitude) (magnitude-operation base-arithmetic))
        (else (invalid-operation opname))))))


(define (vector-additive-operation opname base)
  (simple-operation
    opname
    vector?
    (arity-restrict
      (lambda vectors
        (apply vector-map
               (opname->procedure opname base)
               vectors)))))


(define (scale-operation base)
  (make-operation
    'scalar-product
    (lambda (args) (and (= (length args) 2)
                        ((arithmetic-domainp base) (car args))
                        (vector? (cadr args))))
    (lambda (c v)
      (vector-map (lambda (x) ((opname->procedure '* base) c x))
                  v))))

(define (dot-operation base)
  (simple-operation
    'dot-product
    vector?
    (arity-restrict
      (dot-product-maker (opname->procedure '+ base)
                         (opname->procedure '* base)))))


(define (magnitude-operation base)
  (simple-operation
    'magnitude
    vector?
    (vector-magnitude-maker (opname->procedure '+ base)
                            (opname->procedure '* base)
                            (opname->procedure 'sqrt base))))

(define (invalid-operation opname)
  (simple-operation
    opname
    vector?
    (lambda args (error "Invalid operation on vectors"))))


; Operation: (addition, negation, subtraction, dot product) these-are-arity restriction scalar product, magnitude

(define (arity-restrict vector-op)
  (lambda vectors
    (if (all-equal? (map vector-length vectors))
      (apply vector-op vectors)
      (error "Vectors not of same dimension"))))

(define (vector-magnitude-maker + * sqrt)
  (let ((dot (dot-product-maker + *)))
    (lambda (v)
      (sqrt (dot v v)))))


(define (dot-product-maker + *)
  (lambda (v1 v2) 
    (vector-accumulate + (vector-map * v1 v2))))
                            

; We assume vectors are of dimension at least 1
; If constants are properly set up, then, we can use the additive identity
(define (vector-accumulate proc v)
  (define (accumulate i value)
    (if (n:< i (vector-length v))
      (accumulate (n:+ i 1) (proc value (vector-ref v i)))
      value))
  (accumulate 1 (vector-ref v 0)))


(define (all-equal? lst)
  (or (null? lst)
      (forall (lambda (x) (= x (car lst)))
              lst)))

      


