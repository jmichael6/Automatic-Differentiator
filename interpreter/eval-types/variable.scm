
(define-generic-procedure-handler
  g:eval
  (match-args variable? environment?)
  lookup-variable-env)


(define (variable? x) (symbol? x))
