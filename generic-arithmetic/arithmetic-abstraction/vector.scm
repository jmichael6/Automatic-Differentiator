

(define (vector-extender base-arithmetic)
  (define (fetch opname)
    (arithmetic-procedure opname base-arithmetic))

  (make-arithmetic
    'vector
    vector?
    (list base-arithmetic)

    (lambda (cname base-constant)
      base-constant)

    (lambda (opname base-operation)
      (case opname
        ((+ - neg)
         (vector-additive-operation opname fetch))
        ((*)
         (operation-union
           '*
           (dot-operation fetch)
           (scalar-multiplication-operation 
             (arithmetic-domain-predicate base-arithmetic)
             fetch)))
        ((magnitude)
         (magnitude-operation fetch))
        (else
          (simple-operation
              opname
              vector?
              (lambda args
                (error "Invalid operation applied on vectors"))))))))


(define (vector-additive-operation opname fetch)
  (simple-operation
    opname
    vector?
    (length-restrict 
      (lambda vectors
        (apply vector-map
               (fetch opname)
               vectors)))))

(define (dot-operation fetch)
  (simple-operation
    '*
    vector?
    (length-restrict
      (dot-product-maker (fetch '+) (fetch '*)))))

(define (scalar-multiplication-operation scalar-predicate fetch)
  (make-operation
    '*
    (make-applicability
      (make-case scalar-predicate vector?))
    (lambda (c v)
      (let ((mul (fetch '*)))
        (vector-map (lambda (x) (mul c x))
                    v)))))

(define (magnitude-operation fetch)
  (simple-operation
    'magnitude
    vector?
    (magnitude-maker (fetch '+) (fetch '*) (fetch 'sqrt))))

(define (length-restrict vector-procedure)
  (lambda vectors
    (if (all-equal? (map vector-length vectors))
      (apply vector-procedure vectors)
      (error "Procedure expects vectors of equal length"))))

(define (dot-product-maker + *)
  (lambda (u v)
    (vector-accumulate + (vector-map * u v))))

(define (magnitude-maker + * sqrt)
  (let ((dot (dot-product-maker + *)))
    (lambda (v) 
      (sqrt (dot v v)))))

(define (all-equal? lst)
  (or (null? lst)
      (all (map (lambda (x) (eqv? x (car lst)))
                lst))))

(define (vector-accumulate proc v)
  (define (folder i res)
    (if (n:< i (vector-length v))
      (folder (n:+ i 1) (proc res (vector-ref v i)))
      res))
  (folder 1 (vector-ref v 0)))

(define (n:< . args) (apply (implementation-value '<) args))
(define (n:+ . args) (apply (implementation-value '+) args))

     


           


