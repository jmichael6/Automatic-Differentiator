
(load-dir "../applicability-abstraction")

(load "generic")
(load "metadata")
(load "simple-generic")


(define store (make-simple-dispatch-store))
(define add (store 'add-handler!))
(define get (store 'get-handler))
(define (rules) (store 'rules))
