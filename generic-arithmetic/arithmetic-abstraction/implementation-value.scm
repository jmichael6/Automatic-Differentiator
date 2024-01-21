
(define (implementation-value symb)
  (if (environment-bound? system-global-environment symb)
    (environment-lookup system-global-environment symb)
    (error "No implementation value found")))
