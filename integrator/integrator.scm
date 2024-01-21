
; Stormer Integration

(define (stormer F h)
    (lambda (history)
      (+ (* 2 (x history 0))
         (-   (x history 1))
         (* (/ (expt h 2) 12)
            (+ (* 13 (F (t history 0) (x history 0)))
               (* -2 (F (t history 1) (x history 1)))
               (F (t history 2) (x history 2)))))))


(define (stepper integrator h)
  (lambda (history)
    (extend-history (+ (t history 0) h)
                    (integrator history)
                    history)))

(define (evolver F h make-integrator)
  (let ((integrator (make-integrator F h)))
    (let ((step (stepper integrator h)))
      (define (evolve history n-steps)
        (if (n:> n-steps 0)
          (step (evolve history (n:- n-steps 1)))
          history))
      evolve)))


(define (extend-history t x history)
  (cons (cons t x) history))

(define (t history steps)
  (car (list-ref history steps)))

(define (x history steps)
  (cdr (list-ref history steps)))

(define (make-initial-history start-t h x0 x1 x2)
  (extend-history start-t x0
  (extend-history (- start-t h) x1
  (extend-history (- start-t (* 2 h)) x2
  (make-empty-history)))))
                  
(define (make-empty-history) '())



