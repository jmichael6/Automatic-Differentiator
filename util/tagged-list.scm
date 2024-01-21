
(define (tagged-list symb thing) 
  (and (pair? thing) (eq? (car thing) symb)))
