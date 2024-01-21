

(define (all-args num-args predicate)
  (list 
    (map (lambda (n) predicate)
         (enumerate-interval 1 num-args))))


(define (any-arg num-args special rest)
  (map (lambda (subset)
         (map (lambda (elt)
                (if (n:= elt 1) special rest))
              subset))
       (enumerate-nonempty-subsets num-args)))


; Making applicabilities

(define (make-case . predicates)
  predicates)

(define (make-applicability . cases)
  cases)

(define (match-args . predicates)
  (apply make-applicability 
         (apply make-case predicates)))


; Matching


(define (predicates-match? predicates args)
  (and (n:= (length predicates) (length args))
       (all (map (lambda (p x) (p x))
                 predicates
                 args))))

(define (applicability-union applicabilities)
  (flatmap (lambda (x) x) applicabilities))


(define (n:= . args) 
  (apply (implementation-value '=)
         args))

