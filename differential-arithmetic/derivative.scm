; Making new infinitesimals

(define (make-infinitesimal)
  (make-differential
    (list (make-term 1 (list (make-new-factor))))))



; General method of taking derivatives

(define ((derivative f) x)
  (let* ((dx (make-infinitesimal))
         (dual (f (+ x dx))))
    (extract-coefficient (some-factor dx) dual)))


; Partial derivative


(define ((partial i) f)
  (lambda args
    (assert (and (> i 0) (<= i (length args))))
    ((derivative 
       (apply (apply-except i f) (list-remove i args)))
     (ith-element i args))))

; You really need to understand the difference between applying and applying!!!
; Stop making the same mistake over and over again

; Unary maps and binary on dual numbers

(define (show . things)
  (for-each (lambda (thing)
              (display thing)
              (newline))
            things))

(define (diff:unary f df)
  (lambda (dual)
    (let* ((dx (some-factor dual))
           (finite (finite-part dual dx))
           (infinitesimal (infinitesimal-part dual dx)))
      
      (d:+ (f finite)
           (d:* (df finite) infinitesimal)))))



(define (diff:binary f df0 df1)
  (lambda (a b)
    (let* ((dx (some-factor a b))
           (finite0 (finite-part a dx))
           (inf0 (infinitesimal-part a dx))
           (finite1 (finite-part b dx))
           (inf1 (infinitesimal-part b dx)))
      
     (d:+ (f finite0 finite1)
           (d:+ (d:* (df0 finite0 finite1) inf0)
                (d:* (df1 finite0 finite1) inf1))))))

; Which factor is some factor exactly??

(define (some-factor . differentials)
  (let* ((order (apply max (map differential-order differentials)))
         (left (filter (lambda (x) (= (differential-order x) order))
                       differentials))
         (factors (flatmap (lambda (diff)
                             (let ((terms (differential-terms diff)))
                               (flatmap term-factors terms)))
                           differentials)))
    (if (null? factors)
      (error "No factor found")
      (car factors))))



      
; A little currying librayry 

(define (apply-except i f)
  (lambda args
    (assert (and (> i 0) (<= i (+ 1 (length args)))))
    (lambda (x)
      (apply f (insert-list x i args)))))

(define (insert-list x i args)
  (cond ((= i 1) (cons x args))
        ((null? args) (error "Insert out of range"))
        (else (cons (car args) 
                    (insert-list x (- i 1) (cdr args))))))

(define (list-remove i lst)
  (cond ((null? list) (error "Delete out of range"))
        ((= i 1) (cdr lst))
        (else (cons (car lst)
                    (list-remove (- i 1) (cdr lst))))))

(define (ith-element i lst)
  (cond ((null? lst) (error "Select out of range"))
        ((= i 1) (car lst))
        (else (ith-element (- i 1) (cdr lst)))))


