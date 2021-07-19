(define-module (griddy draw-chickadee)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (chickadee math vector)
  #:use-module (chickadee graphics path)
  #:use-module (chickadee graphics color)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:export (draw-world))

;; TODO: Possible to use GOOPS?

(define *draw/road-junction/size* 25)
(define *draw/road-junction/color* tango-aluminium-6)
(define *draw/road-segment/width* 40)
(define *draw/road-segment/color* tango-aluminium-5)
(define *draw/actor/size* 10)
(define *draw/actor/color* "yellow")

(define wrap-canvas (compose draw-canvas make-canvas))

(define (draw-road-junction junction)
  (wrap-canvas (with-style ((fill-color *draw/road-junction/color*))
     (fill (circle (get junction 'pos)
                   *draw/road-junction/size*)))))

(define (draw-road-segment segment)
  (define (draw-road-lane lane)
    )
  ;; TODO: Use filled polygon?
  (wrap-canvas ((with-style ((stroke-width *draw/road-segment/width*)
                             (stroke-color *draw/road-segment/color*))
                  (stroke (line (get segment 'start-junction 'pos)
                                (get segment 'stop-junction 'pos)))))))

(define (draw-actor actor)
  (define pos (vec2 (get-pos-x actor) (get-pos-y actor)))
  (wrap-canvas (with-style ((fill-color green))
                 (fill (circle pos 10.0)))))

(define (draw-world world)
  (for-each draw-road-segment (get-road-segments world))
  (for-each draw-road-junction (get-road-junctions world))
  (for-each draw-actor (get-actors world))
  )
