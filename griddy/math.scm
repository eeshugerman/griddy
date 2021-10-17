(define-module (griddy math)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
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
            griddy:*
            griddy:+
            griddy:-
            math:extend-primitives!))

(define pi/4 (/ pi 4))

(define (/2 x) (/ x 2))

(define (l2 v1 v2)
  (vec2-magnitude (vec2- v1 v2)))

(define (vec2-rotate vec angle)
  (matrix3-transform (matrix3-rotate angle) vec))

(define (recip x)
  (/ 1 x))

(define origin (vec2 0 0))

(define <vec2> (class-of (vec2 0 0)))

(define-method (griddy:* (num <number>) (vec <vec2>))
  (vec2* vec num))

(define-method (griddy:* (vec <vec2>) (num <number>))
  (griddy:* num vec))

(define-method (griddy:* (a <vec2>) (b <vec2>))
  (throw 'type-error 'griddy:* a b))

(define-method (griddy:* (a <number>) (b <number>))
  ((@ (guile) *) a b))

(define-method (griddy:* . args)
  (fold griddy:*
        (car args)
        (cdr args)))

(define (griddy:+ . args)
  (fold
   (match-lambda*
     [((? vec2? a) (? vec2? b))
      (vec2+ a b)]
     [((? number? a) (? number? b))
      ((@ (guile) +) a b)]
     [(a b)
      (throw 'type-error 'griddy:+ a b)])
   (car args)
   (cdr args)))

(define (griddy:- . args)
  (apply griddy:+ (cons (car args)
                        (map (cut griddy:* -1 <>) (cdr args)))))

;; why can't we just extend `*', `+', `-' directly?
;; issues with `merge-generics'...
(define-macro (math:extend-primitives!)
  (cons 'begin
        (map (lambda (primitive)
               `(define ,primitive ,(symbol-append 'griddy: primitive)))
             '(+ * -))))
