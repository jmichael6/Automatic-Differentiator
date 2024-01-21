
; Matrix extender

(define (matrix-extender base-arithmetic)
  (define (fetch opname)
    (arithmetic-procedure opname base-arithmetic))

  (make-arithmetic
    'matrix matrix? (list base-arithmetic)

    (lambda (cname base-constant)
      base-constant)

    (lambda (opname base-operation)
      (case opname
        ((+ - neg)
         (matrix-additive-operation opname fetch))
        ((*)
         (operation-union
           '*
           (matrix-multiply-operation fetch)
           (scalar-multiplication-operation 
             (arithmetic-domain-predicate base-arithmetic)
             fetch)))
        ((magnitude)
         (magnitude-operation fetch))
        (else
          (simple-operation
            opname
            matrix?
            (lambda args
              (error "Invalid operation applied on matrices"))))))))


; Extender Helper functions

(define (matrix-additive-operation opname fetch)
  (simple-operation
    opname 
    matrix?
    (lambda matrices
      (apply matrix-map 
             (fetch opname)
             matrices))))

(define (matrix-multiply-operation fetch)
  (simple-operation
    '*
    matrix?
    (lambda (A B) 
      (matrix-multiply 
        A B (dot-product-maker (fetch '+) (fetch '*))))))


(define (scalar-multiplication-operation scalar-predicate fetch)
  (make-operation
    '*
    (make-applicability 
      (make-case scalar-predicate matrix?))
    (lambda (c M)
      (let ((mul (fetch '*)))
        (matrix-map (lambda (x) (mul c x)) M)))))

(define (magnitude-operation fetch)
  (simple-operation
    'magnitude
    matrix?
    (lambda (M)
      ((magnitude-maker (fetch '+) (fetch '*) (fetch 'sqrt))
       (matrix->vector M)))))

; Start of the matrix library

; this predicate is not really good enough, but should do for now...
(define (matrix? M) (vector? M))

(define (matrix-num-rows matrix)
  (vector-length matrix))

(define (matrix-num-cols matrix)
  (vector-length (vector-ref matrix 0)))

(define (matrix-map proc . matrices)
  (if (and (all-equal? (map matrix-num-rows matrices))
           (all-equal? (map matrix-num-cols matrices)))
    (apply vector-map 
           (lambda vectors
                   (apply vector-map 
                          (lambda args
                            (apply proc args))
                          vectors))
           matrices)
    (error "All matrices must be of same dimension")))


(define (matrix-row matrix i)
  (vector-ref matrix i))

(define (matrix-col matrix j)
  (vector-map (lambda (v) (vector-ref v j))
              matrix))

(define (matrix-multiply A B dot) 
  (if (not (eqv? (matrix-num-cols A) (matrix-num-rows B)))
    (error "Matrices A and B cannot be multiplied")
    (matrix-construct-by-func
      (lambda (i j) 
        (dot (matrix-row A i) (matrix-col B j))) ; I need dot! strange..
      (matrix-num-rows A)
      (matrix-num-cols B))))

(define (matrix-construct-by-func func rows cols)
  (if (and (n:= rows 1) (n:= cols 1))
    (func 0 0)
    (vector-map 
      (lambda (i)
        (vector-map 
          (lambda (j)
            (func i j))
          (list->vector (enumerate-interval 0 (n:- cols 1)))))
      (list->vector (enumerate-interval 0 (n:- rows 1))))))

(define (n:- . args) (apply (implementation-value '-) args))
(define (n:= . args) (apply (implementation-value '=) args))


(define (matrix->vector M)
  (cond ((n:= (matrix-num-rows M) 1)
         (matrix-row M 0))
        ((n:= (matrix-num-cols M) 1)
         (matrix-col M 0))
        (else
         (error "The matrix cannot be cast into a vector!"))))


; Helper functions for the matrix library

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



; Constructors for matrices

(define (row . elements)
  (vector (list->vector elements)))

(define (col . elements)
  (apply vector
         (map (lambda (x) (vector x))
              elements)))

(define (matrix type . sub-matrices)
  (case type
    ((rows)
       (if (all-equal? (map matrix-num-cols sub-matrices))
         (concat-vertically sub-matrices)
         (error "sub-matrices must have same column-size")))
    ((cols)
       (if (all-equal? (map matrix-num-rows sub-matrices))
          (concat-horizontally sub-matrices)
          (error "sub-matrices must have same row-size")))
    (else
        (error "Unknown type passed to matrix"))))


(define (concat-vertically sub-matrices)
  (apply vector-append sub-matrices))

(define (concat-horizontally sub-matrices)
  (apply vector-map 
         (lambda columns
           (apply vector-append columns))
         sub-matrices))
  

