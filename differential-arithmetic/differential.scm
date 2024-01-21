
; Procedure for making a differential from termlist

(define differential-tag 'differential)

(define (make-differential terms)
  (assert (all (map term? terms)))
  (let ((terms (filter (lambda (term)
                           (not (eq? (term-coefficient term) 0)))
                         terms)))
   (cond ((null? terms) 0)
         ((and (null? (cdr terms))
               (null? (term-factors (car terms)))
               (term-coefficient (car terms))))
         (else (cons differential-tag terms)))))

(define (differential? thing)
  (and (pair? thing) (eq? (car thing) differential-tag)))



; Procedure for getting the termlist from a differential
                                        

(define (differential-terms thing)
  (if (differential? thing)
    (cdr thing)
    (list (make-term thing '()))))



; Defining a polynomial algebra on term lists

(define (d:+ x y)
  (make-differential 
    (+term-lists (differential-terms x)
                 (differential-terms y))))

(define (d:* x y)
  (make-differential
    (*term-lists (differential-terms x)
                 (differential-terms y))))

(define (+term-lists a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((term:< (car a) (car b))
         (cons (car a) (+term-lists (cdr a) b)))
        ((term:< (car b) (car a))
         (cons (car b) (+term-lists a (cdr b))))
        (else
         (cons (make-term (+ (term-coefficient (car a))
                             (term-coefficient (car b)))
                          (term-factors (car a)))
               (+term-lists (cdr a) (cdr b))))))
  

(define (*term-lists a b)
  (accumulate +term-lists '()
    (map (lambda (term-a) 
       (flatmap (lambda (term-b)
          (let ((x (term-factors term-a))
                (y (term-factors term-b)))
            (if (null? (factor-intersection x y))
              (list (make-term (* (term-coefficient term-a)
                                  (term-coefficient term-b))
                               (factor-union x y)))
              '())))

            b))
         a)))

; Finite and infinitesimal parts

(define (finite-part diff factor)
  (if (not (differential? diff))
    diff
    (let ((terms (differential-terms diff)))
      (make-differential
        (filter (lambda (term) (not (has-factor? term factor)))
                terms)))))

(define (infinitesimal-part diff factor)
  (if (not (differential? diff))
    0
    (let ((terms (differential-terms diff)))
      (make-differential
            (filter (lambda (term) (has-factor? term factor))
                    terms)))))


; Getting the factors of highest-order

(define (differential-order diff)
  (let ((terms (differential-terms diff)))
    (apply max (map term-order terms))))

(define (highest-order-factors diff)
  (let* ((terms (differential-terms diff))
         (highest-order (differential-order diff))
         (high-terms (filter (lambda (term) (= (term-order term)
                                               highest-order))
                             terms)))
    (remove-duplicates 
      equal?
      (flatmap (lambda (term) (term-factors term))
               terms))))

(define (remove-duplicates comp lst)
  (if (null? lst)
    '()
    (cons (car lst)
          (remove-duplicates 
            comp
            (filter (lambda (x) (comp x (car lst)))
                    (cdr lst))))))



; Substitution

(define (substitute-factor-default new old thing) thing)

(define substitute-factor 
  (simple-generic-procedure
    'substitute-factor 3 substitute-factor-default))
 
(define (substitute-factor-diff new old diff)
  (let ((terms (differential-terms diff)))
    (make-differential
      (sort (map (lambda (term)
                   (make-term 
                     (term-coefficient term)
                     (sort (map (lambda (x) 
                                  (if (equal? x old) new x))
                                (term-factors term))
                           factor:<)))
                 terms)
            term:<))))

(define-generic-procedure-handler
  substitute-factor
  (make-applicability (make-case factor? factor? differential?))
  substitute-factor-diff)


(define (substitute-factor-proc new old proc)
  (lambda args
    (let ((temp (make-new-factor)))
      (substitute-factor old temp
        (substitute-factor new old 
          (apply proc
            (map (lambda (arg)
                   (substitute-factor temp old arg))
                 args)))))))

(define-generic-procedure-handler
  substitute-factor
  (make-applicability (make-case factor? factor? procedure?))
  substitute-factor-proc)



; Extracting coefficients

(define (extract-coefficient-default factor thing) 0)

(define extract-coefficient
  (simple-generic-procedure
    'extract-coefficient 2 extract-coefficient-default))


(define (extract-coefficient-diff factor diff)
  (let ((terms (differential-terms diff)))
    (make-differential
       (map (lambda (term) (remove-factor term factor))
            (filter (lambda (term) (has-factor? term factor))
                    terms)))))

(define-generic-procedure-handler
  extract-coefficient
  (make-applicability (make-case factor? differential?))
  extract-coefficient-diff)

(define (extract-coefficient-func factor func)
  (lambda args
    (let ((temp (make-new-factor)))
      (substitute-factor factor temp
        (extract-coefficient factor
          (apply func
            (map (lambda (arg) 
                   (substitute-factor temp factor arg))
                 args)))))))

(define-generic-procedure-handler
  extract-coefficient
  (make-applicability (make-case factor? procedure?))
  extract-coefficient-func)



; Helpers

(define (has-factor? term factor)
  (some (map (lambda (x)
               (equal? x factor))
             (term-factors term))))

(define (remove-factor term factor)
  (make-term (term-coefficient term)
             (filter (lambda (x) (not (equal? x factor)))
                     (term-factors term))))

#|

In order to extract the coefficient from a function 
What do we do?

This is the first important key point.

  We are only interested in the functions that extract-dx will generate
  Each invocation of this function should work over a new separate algebra
  The factor is a NEW ONE and DIFFERENT from all others
  This is something that we should clearly remember.

This is the second important key point
  
  Differentials factors are generated early. This is not possible to avoid really. Or is it?
  Each invocation of this function will use the same differential.
  This is the second thing we should clearly remember.

This is the third important key point
  
  The extract-coefficient is a curious kind of function.
  It has several nice properties that allow us to find a neat a solution
  
  First remap the factors in the arguments to prevent collisions
  Then apply the function using the previously generated factor and extract the coefficient
  Finally undo the mapping to restore the original factors

  The nice thing is
    1) It is invisible to the outside
    2) After extracting, the coefficient is no longer present
  
So why do I need this other function??
  
  Hmm... because of the way we understand substitution in a function 
  It is a subtle idea, substituting in a function.
  But it can be cleverly defined using the same trick exactly

|#
