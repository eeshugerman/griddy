(define-module (griddy math)
  #:use-module (srfi srfi-1)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
  #:re-export (pi pi/2)
  #:export (pi/4
            /2
            l2
            vec2-rotate
            vec2+/many))

(define pi/4 (/ pi 4))

(define (/2 x) (/ x 2))

(define (l2 v1 v2)
  (vec2-magnitude (vec2- v1 v2)))

(define (vec2-rotate vec angle)
  (matrix3-transform (matrix3-rotate angle) vec))

(define (vec2+/many . vecs)
  (fold vec2+ (car vecs) (cdr vecs)))
