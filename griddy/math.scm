(define-module (griddy math)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
  #:re-export (pi pi/2)
  #:export (pi/4
            /2
            recip
            l2
            origin
            vec2-rotate
            vec2+/many)
  #:replace (+ * -))

(define pi/4 (/ pi 4))

(define (/2 x) (/ x 2))

(define (l2 v1 v2)
  (vec2-magnitude (vec2- v1 v2)))

(define (vec2-rotate vec angle)
  (matrix3-transform (matrix3-rotate angle) vec))

(define (vec2+/many . vecs)
  (fold vec2+ (car vecs) (cdr vecs)))

(define (recip x)
  (/ 1 x))

(define origin (vec2 0 0))

(define (* . args)
  (fold
   (match-lambda*
     [(or ((? vec2? vec2) (? number? num))
          ((? number? num) (? vec2? vec2)))
      (vec2* vec2 num)]
     [((? number? a) (? number? b))
      ((@ (guile) *) a b)]
     [(a b)
      (throw 'type-error '* a b)])
   (car args)
   (cdr args)))

(define (+ . args)
  (fold
   (match-lambda*
     [((? vec2? a) (? vec2? b))
      (vec2+ a b)]
     [((? number? a) (? number? b))
      ((@ (guile) +) a b)]
     [(a b)
      (throw 'type-error '+ a b)])
   (car args)
   (cdr args)))

(define (- . args)
  (apply + (cons (car args)
                 (map (cut * -1 <>) (cdr args)))))

