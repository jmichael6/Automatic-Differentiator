

; Constructors and Selectors for arithmetics

(define (make-arithmetic name domain-predicate base-arithmetics constant-map operation-map)
  (list 'arithmetic name domain-predicate base-arithmetics constant-map operation-map))


(define (arithmetic-opmap arithmetic)
  (list-ref arithmetic 5))

(define (arithmetic-bases arithmetic)
  (list-ref arithmetic 3))

(define (arithmetic-domainp arithmetic)
  (list-ref arithmetic 2))


; The Recursive construction of an operation for an arithmetic

(define (opname->operation opname arithmetic)
  (let ((base-ops (map (lambda (base) (opname->operation opname base))
                       (arithmetic-bases arithmetic))))
    (apply (arithmetic-opmap arithmetic)
           (cons opname base-ops))))

(define (opname->procedure opname arithmetic)
  (operation-procedure (opname->operation opname arithmetic)))





; Installation of an arithmetic
; *** HARD CODING ALERT *** (this needs fixing and thinking)
; And we hit a problem immediately

(define (install-arithmetic! arithmetic) 
  (define (add x . rest) 
    (if (null? rest)
      x
      ((opname->procedure '+ arithmetic) x (apply add rest))))

  (define (subtract x . rest)
    (if (null? rest)
      ((opname->procedure 'neg arithmetic) x)
      ((opname->procedure '- arithmetic) x (apply add rest))))

  (define (multiply x . rest)
    (if (null? rest)
      x
      ((opname->procedure '* arithmetic) x (apply multiply rest))))

  (define (divide x . rest)
    (if (null? rest)
      ((opname->procedure 'inv arithmetic) x)
      ((opname->procedure '/ arithmetic) x (apply multiply rest))))

  (define (default opname) 
    (lambda args
      (apply (opname->procedure opname arithmetic) args)))

  (map (lambda (symb)
         (let ((binding (assoc symb `((+ ,add) (- ,subtract) (* ,multiply) (/ ,divide)))))
           (let ((proc (if binding (cadr binding) (default symb))))
             (eval `(set! ,symb ,proc) (interaction-environment)))))
       '(+ - * / expt sin cos sqrt magnitude)))


; This hardcoding is getting a little painful

(define (operator-arity opname)
  (case opname
    ((inv neg sin cos sqrt magnitude) 1)
    ((expt) 2)
    (else 2)))

; *** Hardcoding *** 
(define (opname->symbol opname)
  (case opname
    ((inv) '/)
    ((neg) '-)
    (else opname)))






             


