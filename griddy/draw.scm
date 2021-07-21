(define-module (griddy draw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
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

(define with-canvas (compose draw-canvas make-canvas))

(define (draw-road-junction junction)
  (with-canvas (with-style ((fill-color *draw/road-junction/color*))
                 (fill (circle (get junction 'pos)
                               *draw/road-junction/size*)))))

(define (vec2-rotate vec angle)
  (matrix3-transform (matrix3-rotate angle) vec))

(define (draw-road-segment segment)

  (let* ((v-start (get segment 'start-junction 'pos))
         (v-stop (get segment 'stop-junction 'pos))
         (v-segment (vec2- v-stop v-start))
         (v-tangent (vec2-normalize v-segment))
         (v-ortho (vec2-rotate v-tangent (/ pi 4)))
         (v-to-edge (vec2* v-ortho (/ *draw/road-segment/width* 2)))
         (p-1 (vec2+ v-start v-to-edge))
         (p-2 (vec2- v-start v-to-edge))
         (p-3 (vec2- v-stop v-to-edge))
         (p-4 (vec2+ v-stop v-to-edge))
         (road-painter (fill (polyline p-1 p-2 p-3 p-4 p-1))))

    (define (draw-road-lane lane)
      (match (get lane 'direction)
        ('forw
         (with-style ((stroke-color green))
           (stroke (line (vec2+ v-start (vec2* v-to-edge 1/2))
                         (vec2+ v-stop  (vec2* v-to-edge 1/2))))))
        ('back
         (with-style ((stroke-color red))
           (stroke (line (vec2- v-start (vec2* v-to-edge 1/2))
                         (vec2- v-stop  (vec2* v-to-edge 1/2))))))))

    (let* ((lane-painters (map draw-road-lane (get segment 'lanes))))

      (with-canvas (apply superimpose road-painter lane-painters)))))

(define (draw-actor actor)
  (define pos (vec2 (get-pos-x actor) (get-pos-y actor)))
  (with-canvas (with-style ((fill-color blue))
                 (fill (circle pos 10.0)))))

(define (draw-world world)
  (for-each draw-road-segment (get-road-segments world))
  (for-each draw-road-junction (get-road-junctions world))
  (for-each draw-actor (get-actors world))
  )
