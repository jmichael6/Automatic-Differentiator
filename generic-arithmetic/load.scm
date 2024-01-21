
(load-dir "../generic-procedures")
(load-dir "../applicability-abstraction")


(load-dir "arithmetic-abstraction")
(load "generic-arithmetic")


; What a beautiful idea extenders are
; They fit very naturally into a combinator language
; But they are even very useful outside it
; How general and simple!

(define g (make-generic-arithmetic make-simple-dispatch-store))

(set! g (arithmetic-extender g numeric-arithmetic))
(set! g (arithmetic-extender g (function-extender g)))
(set! g (arithmetic-extender g (symbolic-extender numeric-arithmetic)))
(set! g (arithmetic-extender g (matrix-extender g)))


; (define (p x) (lambda (y) (cons x y)))
; (define (q y) (lambda (x) (cons x y)))

(install-arithmetic! g)


