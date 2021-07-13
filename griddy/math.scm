(define-module (griddy math)
  #:use-module (chickadee math vector)
  #:export(/2 l2))

(define (/2 x) (/ x 2))

(define (l2 v1 v2)
  (vec2-magnitude (vec2- v1 v2)))
