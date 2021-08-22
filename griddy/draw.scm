(define-module (griddy draw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee graphics path)
  #:use-module (chickadee graphics color)
  #:use-module (griddy core)
  #:use-module (griddy math)
  #:use-module (griddy util)
  #:export (draw-world))

;; TODO: Possible to use GOOPS?

(define *draw/road-junction/size* 40)
(define *draw/road-junction/color* tango-aluminium-6)
(define *draw/road-segment/color* tango-aluminium-5)
(define *draw/road-lane/arrow-size* 5)
(define *draw/road-lane/color* tango-plum)
(define *draw/actor/size* 10)
(define *draw/actor/color* tango-sky-blue)

(define with-canvas (compose draw-canvas make-canvas))

(define (angle-of vec)
  (atan (vec2-y vec) (vec2-x vec)))


(define (rotate-in-place angle place painter)
  ;; why does this work?? should be backwards
  ((compose
    (cut translate (vec2* place -1) <>)
    (cut rotate angle <>)
    (cut translate place <>))
   painter))

(define (draw-road-junction junction)
  (with-canvas
   (with-style ((fill-color *draw/road-junction/color*))
     (rotate-in-place pi/4
                      (get junction 'pos)
                      (fill (regular-polygon (get junction 'pos)
                                             4
                                             *draw/road-junction/size*))))))


(define (draw-road-segment segment)
  (let* ((v-start (get segment 'start-junction 'pos))
         (v-stop (get segment 'stop-junction 'pos))
         (v-to-edge (vec2* (get-v-ortho segment)
                           (/2 (get-width segment))))
         (p-1 (vec2+ v-start v-to-edge))
         (p-2 (vec2- v-start v-to-edge))
         (p-3 (vec2- v-stop v-to-edge))
         (p-4 (vec2+ v-stop v-to-edge))
         (road-painter (fill (polyline p-1 p-2 p-3 p-4 p-1))))

    (define (draw-road-lane lane)
      (let* ((direction (get lane 'direction))
             (v-lane-offset (get-offset lane))
             (v-segment (vec2- v-stop v-start))
             (v-arrow-pos
              (vec2+/many v-start
                          (vec2* v-segment 1/2)
                          v-lane-offset))
             (v-lane
              (vec2* v-segment (match direction ('forw 1) ('back -1))))
             (arrow-painter
              ;; `rotate' rotates clockwise (?!), triangle starts
              ;; pointing upwards
              (rotate-in-place (* -1 (- (angle-of v-lane) pi/2))
                               v-arrow-pos
                               (fill (regular-polygon v-arrow-pos 3
                                                      *draw/road-lane/arrow-size*))))
             (line-painter
              (stroke (line (vec2+ v-start v-lane-offset)
                            (vec2+ v-stop v-lane-offset)))))
        (with-style ((stroke-color *draw/road-lane/color*)
                     (fill-color *draw/road-lane/color*))
          (superimpose line-painter arrow-painter))))

    (let* ((lane-painters (map draw-road-lane (get segment 'lanes))))
      (with-canvas (apply superimpose road-painter (reverse lane-painters))))))

(define (draw-actor actor)
  (with-canvas (with-style ((fill-color *draw/actor/color*))
                 (fill (circle (get-pos actor) *draw/actor/size*)))))

(define (draw-world world)
  (for-each draw-road-segment (get-road-segments world))
  (for-each draw-road-junction (get-road-junctions world))
  (for-each draw-actor (get-actors world)))
