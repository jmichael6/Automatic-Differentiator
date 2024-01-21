
; Constructors and Accessors for the metadata

(define (make-metadata name arity store default-handler)
  ((store 'set-default-handler!) default-handler)
  (list 'metadata name arity store))


(define (generic-procedure-dispatch-store generic-procedure)
  (metadata-dispatch-store 
    (generic-procedure-metadata generic-procedure)))

(define (metadata-dispatch-store metadata)
  (list-ref metadata 3))

(define (metadata-default-handler metadata)
  ((metadata-dispatch-store metadata) 'get-default-handler))

(define (metadata-get-handler metadata)
  ((metadata-dispatch-store metadata) 'get-handler))

; The metadata table

(define metadata-table (make-key-weak-eqv-hash-table))

(define (set-generic-procedure-metadata! generic-procedure metadata)
  (hash-table-set! metadata-table generic-procedure metadata))

(define (generic-procedure-metadata generic-procedure)
  (hash-table-ref/default metadata-table generic-procedure #f))


