


The load file I used when working with generic arithmetics

(load-dir "util")
(load-dir "applicability-abstraction")
(load-dir "generic-procedures-util")
(load-dir "generic-arithmetic-system")
(load-dir "integrator")

(define ev (evolver (literal-function 'F) 'h stormer))
(define initial-history (make-initial-history 't 'h 'xt 'xt-h 'xt-2h))
(define history (ev initial-history 2))


(define f (simple-operation '+ number? +))
(define g (simple-operation '+ symbol? (lambda args (cons '+ args))))
(define h (operation-union '+ f g))


