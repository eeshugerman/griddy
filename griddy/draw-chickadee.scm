(define-module (griddy draw-chickadee)
  #:use-module (oop goops)
  #:use-module (chickadee math vector)
  #:use-module (chickadee graphics)
  #:use-module (griddy core)
  #:export (draw))

(define-method (draw (actor <actor>))
  (circle (vec2 (get-pos-x actor) (get-pos-y actor))
          10))

(define-method (draw (world <world>))
  (fold draw (get-actors world)))
