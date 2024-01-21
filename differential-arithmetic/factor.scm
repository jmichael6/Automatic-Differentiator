
; Definitions

(define factor-tag 'factor)

(define (make-factor id)
  (cons factor-tag id))

(define (factor-id factor)
  (cdr factor))

(define (factor? thing) 
  (and (pair? thing) (eq? (car thing) factor-tag)))



; The make new factor function


(define make-new-factor
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      (make-factor counter))))



; Ordering factors

(define (factor:< x y)
  (< (factor-id x) (factor-id y)))

(define (factor:= x y)
  (equal? x y))


; Intersections and unions for factors

(define (ordered-intersection less a b)
  (cond ((null? a) '())
        ((null? b) '())
        ((less (car a) (car b))
         (ordered-intersection less (cdr a) b))
        ((less (car b) (car a))
         (ordered-intersection less a (cdr b)))
        (else
         (cons (car a) (ordered-intersection less (cdr a) (cdr b))))))

(define (ordered-union less a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((less (car a) (car b))
         (cons (car a) (ordered-union less (cdr a) b)))
        ((less (car b) (car a))
         (cons (car b) (ordered-union less a (cdr b))))
        (else
          (cons (car a) (ordered-union less (cdr a) (cdr b))))))

(define (factor-intersection a b)
  (ordered-intersection factor:< a b))

(define (factor-union a b)
  (ordered-union factor:< a b))
