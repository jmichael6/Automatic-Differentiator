
(define (enumerate-interval low high)
  (if (n:> low high)
    '()
    (cons low (enumerate-interval (n:+ low 1) high))))

(define (enumerate-subsets n)
  (if (n:> n 0)
    (let ((prev-subsets (enumerate-subsets (n:- n 1))))
      (flatmap (lambda (subset) (list (cons 0 subset) (cons 1 subset)))
               prev-subsets))
    (list '())))

(define (enumerate-nonempty-subsets n)
  (if (n:> n 1)
    (append (map (lambda (subset) (cons 0 subset))
                 (enumerate-nonempty-subsets (n:- n 1)))
            (map (lambda (subset) (cons 1 subset))
                 (enumerate-subsets (n:- n 1))))
    (list '(1))))


(define (enumerate-nonempty-subsets n)
  (filter (lambda (subset) (n:< 0 (accumulate n:+ 0 subset)))
          (enumerate-subsets n)))


(define (n:+ . args) (apply (implementation-value '+) args))
(define (n:- . args) (apply (implementation-value '-) args))
(define (n:> . args) (apply (implementation-value '>) args))
(define (n:< . args) (apply (implementation-value '<) args))
