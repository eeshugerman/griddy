(define-module (gritty math) #:export(/2 l2))

(define (/2 x) (/ x 2))

(define (l2 x1 y1 x2 y2)
  (define (square x) (expt x 2))
  (let ((dx (- x1 x2))
        (dy (- y1 y2)))
    (sqrt (+ (square dx) (square dy)))))
