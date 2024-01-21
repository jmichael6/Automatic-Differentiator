
(load-dir "../util")
(load-dir "../generic-arithmetic-system")

(load "factor")
(load "term")
(load "differential")
(load "derivative")
(load "arithmetic")

(define g (make-generic-arithmetic make-simple-dispatch-store))

(set! g (arithmetic-extender g numeric-arithmetic))
(set! g (arithmetic-extender g (function-extender g)))
(set! g (arithmetic-extender g (matrix-extender g)))
(set! g (arithmetic-extender g (symbolic-extender numeric-arithmetic)))
(set! g (differential-extender g))
(install-arithmetic! g)

; Rather remeniscent of haskell, isn't it??
(define (sec f) (derivative (derivative f))) 

(define (f x y) (sin (* x y)))
(define fx ((partial 1) f))

; How do I define the second derivative in this framework?

; (define prod (d:* (d:+ 1 dx) (d:+ 1 dy)))
