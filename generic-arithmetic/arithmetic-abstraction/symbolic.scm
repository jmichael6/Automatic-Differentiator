

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 
    'symbolic
    literal?
    (list base-arithmetic)

    (lambda (name base-constant)
      base-constant)

    (lambda (opname base-operation)
      (make-operation
        opname
        (any-arg (operator-arity opname)
                 literal?
                 (arithmetic-domain-predicate base-arithmetic))
        (lambda args
          (let ((simp (simplify opname args)))
            (if (simplify-failed? simp)
              (make-literal (cons opname args))
              simp)))))))


; Hmm... is this really a good idea??
(define (make-literal x) (cons 'lit x))
(define (literal? x) (or (symbol? x) (and (pair? x) (eq? (car x) 'lit))))


; Cleaning literals for display

(define (clean x) (remove-lit x))

(define (remove-lit x)
  (if (pair? x)
    (if (eq? (car x) 'lit)
      (remove-lit (cdr x))
      (cons (remove-lit (car x)) (remove-lit (cdr x))))
    x))



; Simplification

(define simplify-failed (default-object))
(define simplify-failed? default-object?)

(define (simplify opname args)
  (case opname
    ((*) (apply simplify-mul args))
    ((+) (apply simplify-add args))
    ((-) (apply simplify-sub args))
    ((/) (apply simplify-div args))
    (else simplify-failed)))

(define (simplify-mul x y)
  (cond ((or (zero? x) (zero? y)) 0)
        ((one? x) y)
        ((one? y) x)
        (else simplify-failed)))

(define (simplify-add x y)
  (cond ((zero? x) y)
        ((zero? y) x)
        (else simplify-failed)))

(define (simplify-sub x y)
  (cond ((zero? x) (make-literal (list 'neg y)))
        ((zero? y) x)
        (else simplify-failed)))

(define (simplify-div x y)
  (cond ((one? y) x)
        (else simplify-failed)))

(define (zero? x) (and (number? x) (= x 0)))
(define (one? x) (and (number? x) (= x 1)))

