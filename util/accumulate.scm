
(define (accumulate proc null-value lst)
  (if (null? lst)
    null-value
    (proc (car lst) (accumulate proc null-value (cdr lst)))))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst)) 
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))


(define (flatmap proc lst)
  (accumulate append '()
     (map proc lst)))


(define (all lst)
  (accumulate (lambda (x y) (and x y)) #t lst))


(define (some lst)
  (accumulate (lambda (x y) (or x y)) #f lst))



