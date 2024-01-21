
(define (add-arithmetics . base-arithmetics)
  (if (null? (cdr base-arithmetics))
    (car base-arithmetics)

    (make-arithmetic
      'add
      (disjoin* (map arithmetic-domainp base-arithmetics))
      base-arithmetics
      constant-union
      operation-union)))

(define (operation-union opname . base-operations)
  (make-operation
    opname
    (applicability-union (map operation-applicability base-operations))
    (lambda args
      (operation-dispatch base-operations args))))


(define (operation-dispatch base-operations args)
  (let ((op (find (lambda (op) ((operation-applicability op) args))
                  base-operations)))
    (if (not op)
      (error "No applicable operation found")
      (apply (operation-procedure op) args))))

(define (constant-union name . base-constants)
  (let ((filtered (remove default-object? base-constants)))
    (if (null? filtered)
      (default-object) 
      (car filtered))))


; Useful variations

(define (extend-arithmetic extender base-arithmetic)
  (add-arithmetics base-arithmetic (extender base-arithmetic)))



; Helpers

(define (disjoin* predicates)
  (apply disjoin predicates))

(define (disjoin . predicates)
  (lambda (x) 
    (some (lambda (p) (p x)) predicates)))

(define (some f lst)
  (cond ((null? lst) false)
        ((f (car lst)) true)
        (else (some f (cdr lst)))))

(define (applicability-union apps)
  (disjoin* apps))

