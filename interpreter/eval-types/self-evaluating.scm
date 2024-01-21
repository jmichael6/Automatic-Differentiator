

; Self evaluating forms

(define-generic-procedure-handler
  g:eval
  (match-args self-evaluating? environment?)
  (lambda (expression environment)
    expression))


; Self-evaluating syntax

(define (self-evaluating expr)
  (or (number? expr)
      (boolean? expr)
      (string? expr)))

 
