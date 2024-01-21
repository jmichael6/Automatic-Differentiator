
; The definition of an operation

(define (make-operation opname applicability procedure)
  (list 'operation opname applicability procedure))


(define (operation-procedure operation)
  (list-ref operation 3))

(define (operation-applicability operation)
  (list-ref operation 2))


; Simple operations

(define (simple-operation opname predicate procedure)
  (make-operation
    opname
    (all-args (operator-arity opname) predicate)
    procedure))

; We need to take the union of multiple operations

(define (operation-union opname . operations)
  (make-operation
    opname
    (applicability-union 
      (map operation-applicability operations))
    (lambda args
      (let ((op (find-applicable-operation operations args)))
        (apply (operation-procedure op)
               args)))))

(define (find-applicable-operation operations args)
  (find (lambda (op) (operation-applicable? op args))
        operations))

(define (operation-applicable? operation args)
  (some (map (lambda (predicates) 
              (predicates-match? predicates args))
             (operation-applicability operation))))

        


