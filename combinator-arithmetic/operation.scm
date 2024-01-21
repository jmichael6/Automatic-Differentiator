; Constructors and Selectors for operations

(define (make-operation opname applicability proc)
  (list 'operation opname applicability proc))


(define (operation-applicability operation)
  (list-ref operation 2))

(define (operation-procedure operation)
  (list-ref operation 3))

(define (apply-operation operation args)
  (apply (operation-procedure operation)
         args))


; The Definition of a simple operation

(define (simple-operation opname predicate proc)
  (make-operation opname
                  (all-args (operator-arity opname)
                            predicate)
                  proc))




; Defining convenient applicability interfaces

(define (all-args arity predicate) 
  (lambda (args) 
    (and (= (length args) arity)
         (forall predicate args))))


(define (any-arg arity special? rest?)
  (lambda (args) 
    (and (= (length args) arity)
         (forsome special? rest? args))))


; Helper procedures for applicability

(define (forall predicate args)
  (cond ((null? args) true)
        ((not (predicate (car args))) false)
        (else (forall predicate (cdr args)))))

(define (forsome special? rest? args)
  (cond ((null? args) false)
        ((rest? (car args)) (forsome special? rest? (cdr args)))
        ((special? (car args)) (forall (lambda (x) (or (special? x) (rest? x))) (cdr args)))
        (else false)))




