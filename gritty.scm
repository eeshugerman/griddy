(use-modules (oop goops))

(define-class <world> ()
  grid
  (road-junctions #:init-form '())
  (road-segments #:init-form '())
  (actors #:init-form '()))

(define-class <grid> ()
  (x-max #:init-keyword #:x-max)
  (y-max #:init-keyword #:y-max)
  arr)

(define-method (initialize (self <grid>) . args)
  (next-method)
  (slot-set! self 'arr (make-array (make <tile>)
                                   (slot-ref self 'x-max)
                                   (slot-ref self 'y-max)))
  self)

(define-class <tile> ()
  road-junction
  (road-segments #:init-form '())
  (actors #:init-form '()))

(define-class <road-junction> ()
  pos-x ;; integer
  pos-y ;; integer
  (road-segments #:init-form '()))

(define-class <road-segment> ()
  start ;; <road-junction>
  stop  ;; <road-junction>
  grid)


(define grid (make <grid> #:x-max 10 #:y-max 10))
