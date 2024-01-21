
; this is the interface
(define (generic-procedure-constructor dispatch-store-maker)
  (lambda (name arity default-handler)
    ;we must also handle the metadata 
    (let ((metadata
            (make-metadata 
              name
              arity
              (dispatch-store-maker)
              (or default-handler
                  generic-procedure-default-error-handler)))) 

      ; this is the main purpose of generic-procedure-constructor
      (define (generic-procedure . args)
        (generic-procedure-dispatch  
          (generic-procedure-metadata generic-procedure) 
          args))

      (set-generic-procedure-metadata! generic-procedure metadata) 
      generic-procedure)))


      
(define (define-generic-procedure-handler generic-procedure
                                          applicability 
                                          handler)
  (((generic-procedure-dispatch-store generic-procedure)
    'add-handler!)
   applicability 
   handler))


(define (generic-procedure-dispatch metadata args)
  (let ((final-handler (or ((metadata-get-handler metadata) args)
                           ((metadata-default-handler metadata)))))
    (apply final-handler args)))



(define (generic-procedure-default-error-handler . args)
  (error "No applicable handler found for generic procedure"))




