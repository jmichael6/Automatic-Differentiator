
; The recursive definition of an arithmetic

(define (make-arithmetic name domain-predicate base-arithmetics constant-map operation-map)
  (list 'arithmetic name domain-predicate base-arithmetics constant-map operation-map))

(define (arithmetic-base-arithmetics arithmetic)
  (list-ref arithmetic 3))

(define (arithmetic-operation-map arithmetic)
  (list-ref arithmetic 5))

(define (arithmetic-constant-map arithmetic)
  (list-ref arithmetic 4))

(define (arithmetic-domain-predicate arithmetic)
  (list-ref arithmetic 2))


; The recursive definition of the finding of an operation

(define (arithmetic-operation opname arithmetic)
  (let ((base-operations (map (lambda (arith) (arithmetic-operation opname arith))
                              (arithmetic-base-arithmetics arithmetic))))
    (apply (arithmetic-operation-map arithmetic)
           opname
           base-operations)))

(define (arithmetic-constant cname arithmetic)
  (let ((base-constants (map (lambda (arith) (arithmetic-constant cname arith))
                             (arithmetic-base-arithmetics arithmetic))))
    (apply (arithmetic-constant-map arithmetic)
           cname
           base-constants)))

; The definition apply through an arithmetic

(define (arithmetic-procedure opname arithmetic)
  (let ((operation (arithmetic-operation opname arithmetic)))
    (if operation
      (operation-procedure operation)
      (error "No operation found in the arithmetic"))))


; The installation of an arithmetic



(define (install-arithmetic! arithmetic)
  (install-operators! arithmetic)
  (install-constants! arithmetic))

(define (install-operators! arithmetic)
  (define (fetch opname) 
    (arithmetic-procedure opname arithmetic))

  (define (id x) x)

  (define (construct opname)
    (cond ((eq? opname 'id) (lambda (x) x))
          ((and (= (operator-arity opname) 2)
                (operator-folding? opname))
           (binary->nary
             (fetch opname)
             (construct (folding-operator-unary-name opname))))
          (else (fetch opname))))

  (map (lambda (opname)
         (let ((val (construct opname)))
           (eval `(define ,opname ,val)
                 (interaction-environment))))
       (map operator-name operators)))


(define (install-constants! arithmetic)
  (define (fetch cname)
    (arithmetic-constant cname arithmetic))

  (map (lambda (cname)
         (let ((val (fetch cname)))
           (eval `(define ,cname ,val)
                 (interaction-environment))))
       constants))



      
; Binary to Nary conversion

(define (binary->nary binary unary)
  (define (reduce a b . rest)
    (if (null? rest)
      (binary a b)
      (apply reduce (binary a b) rest)))

  (define (nary a . rest)
    (if (null? rest)
      (unary a)
      (apply reduce a rest)))
  nary)

; Operators to be made generic


; This is still unsatisfying. It's better, but not yet simple
(define operators
  '((+ 2 id) (* 2 id) (- 2 neg) (/ 2 inv)
    (expt 2) (log 1)
    (neg 1) (inv 1)
    (sin 1) (cos 1)
    (sqrt 1) (magnitude 1)))

(define constants
  '(additive-identity multiplicative-identity))

(define (operator-name operator)
  (car operator))

(define (operator-arity opname)
  (let ((p (assoc opname operators)))
    (second p)))

(define (operator-folding? opname)
  (member opname '(+ - * /)))

(define (folding-operator-unary-name opname)
  (let ((p (assoc opname operators)))
    (third p)))




