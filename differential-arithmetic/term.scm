

;;; Definitions

; Defining a term as a tagged list of a coefficient and factors

(define term-tag 'term)

(define (make-term coeff factors)
  (cons term-tag (cons coeff factors)))


(define (term-coefficient term)
  (cadr term))

(define (term-factors term)
  (cddr term))
   
(define (term? thing)
  (and (pair? thing) (eq? (car thing) term-tag)))




;;; Ordering terms 

(define (term:< a b)
  (let ((x (term-factors a))
        (y (term-factors b)))
    (or (and (= (length x) (length y))
             (dict:< factor:< x y))
        (< (length x) (length y)))))


(define (dict:< less a b)
  (cond ((and (null? a) (null? b)) false)
        ((null? a) true)
        ((null? b) false)
        ((less (car a) (car b)) true)
        ((less (car b) (car a)) false)
        (else (dict:< less (cdr a) (cdr b)))))
 

;;; Terms and orders

(define (term-order term)
  (length (term-factors term)))





