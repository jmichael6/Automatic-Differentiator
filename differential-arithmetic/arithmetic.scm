
; Defining a differential arithmetic

(define (differential-extender generic-arithmetic)
  (define (fetch opname)
    (arithmetic-procedure opname generic-arithmetic))
  (define + (fetch '+))
  (define * (fetch '*))
  (define - (fetch '-))
  (define / (fetch '/))
  (define log (fetch 'log))
  (define sin (fetch 'sin))
  (define cos (fetch 'cos))
  (define neg (fetch 'neg))
  (define (square x) (* x x))

  (make-arithmetic
    'generic any-object?
    (list generic-arithmetic)
    constant-union

    (lambda (opname generic-operation)
      (define-generic-procedure-handler
        (operation-procedure generic-operation)
        (any-arg (operator-arity opname) differential? any-object?)

        (case opname
          ((+) (diff:binary + (lambda (x y) 1) (lambda (x y) 1)))
          ((*) (diff:binary * (lambda (x y) y) (lambda (x y) x)))
          ((-) (diff:binary - (lambda (x y) 1) (lambda (x y) -1)))
          ((/) (diff:binary / (lambda (x y) (/ 1 y))
                              (lambda (x y) (neg (/ x (square y))))))
          ((expt) diff:expt)

          ((log) (diff:unary log (lambda (x) (/ 1 x))))
          ((sin) (diff:unary sin (lambda (x) (cos x))))
          ((cos) (diff:unary cos (lambda (x) (neg (sin x)))))
          ((neg) (diff:unary neg (lambda (x) -1)))
          (else 
            (lambda args 
              (error "Unknown opname passed to diff-arithmetic")))))
      generic-operation)))


(define diff:expt
  (diff:binary 
    expt
    (lambda (b e)
      (* e (expt b (- e 1))))
    (lambda (b e)
      (if (and (number? b) (zero? b))
        (if (number? y)
          (if (positive? y)
            0
            (error "Derivative is undefined: EXPT" x y))
          0)
        (* (log b) (expt b e))))))


