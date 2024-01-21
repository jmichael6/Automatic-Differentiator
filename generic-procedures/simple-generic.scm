
 
(define (make-simple-dispatch-store)
  (let ((default-handler #f) (rules '()))

    (define (get-default-handler) 
      default-handler)

    (define (set-default-handler! handler)
      (set! default-handler handler))

    (define (get-handler args)
      (let ((rule (find (lambda (rule) 
                          (predicates-match? (car rule) args)) 
                        rules))) 
        (and rule (cdr rule))))

    (define (add-handler! applicability handler)
      (for-each (lambda (predicates)
                   (let ((p (assoc predicates rules)))
                     (if p
                       (set-cdr! p handler)
                       (set! rules (cons (cons predicates handler)
                                         rules)))))
                 applicability))

    (lambda (message)
      (case message
       ((add-handler!) add-handler!) 
       ((get-handler) get-handler) 
       ((set-default-handler!) set-default-handler!) 
       ((get-default-handler) get-default-handler) 
       ((rules) rules) 
       (else (error "Invalid message passed to simple-dispatch-store"))))))


(define simple-generic-procedure 
  (generic-procedure-constructor make-simple-dispatch-store))

