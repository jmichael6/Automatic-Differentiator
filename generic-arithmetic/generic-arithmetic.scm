
(define (make-generic-arithmetic dispatch-store-maker)
  (make-arithmetic
    'generic any-object? '()
    constant-union

    (let ((bindings '())) ; this is where the possiblity of recursion comes into play allowing for self-referential structures
      (lambda (opname)
        (if (assoc opname bindings)
          (cdr (assoc opname bindings))
          (let* ((make-generic-procedure (generic-procedure-constructor
                                            dispatch-store-maker))
                 (operation (simple-operation
                              opname
                              any-object?
                              (make-generic-procedure opname
                                                      (operator-arity opname)
                                                      #f))))
              (set! bindings (cons (cons opname operation)
                                   bindings))
              operation))))))
              



; We use default objects since we want a predicate+value function
; But we cannot use true/false
; We need an object that cannot be any constant
; That is the default object
(define (constant-union constant-name . base-constants)
  (let ((filtered (remove default-object? base-constants)))
    (if (null? filtered)
      (default-object)
      (car filtered))))
      
    

(define (arithmetic-extender generic-arithmetic arithmetic)
  (make-arithmetic
    'generic any-object? 
    (list generic-arithmetic arithmetic)
    constant-union

    (lambda (opname generic-operation arithmetic-operation)
      (let ((generic-procedure 
              (operation-procedure generic-operation)))
        (define-generic-procedure-handler
          generic-procedure
          (operation-applicability arithmetic-operation)
          (operation-procedure arithmetic-operation))
        generic-operation))))

(define (extend-generic-arithmetic generic-arithmetic extender)
  (arithmetic-extender generic-arithmetic (extender generic-arithmetic)))









