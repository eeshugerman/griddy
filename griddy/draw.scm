(define-module (griddy draw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (chickadee math)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee graphics path)
  #:use-module (griddy core)
  #:use-module (griddy constants)
  #:use-module (griddy math)
  #:use-module (griddy util)
  #:export (draw-world))

;; TODO: Possible to use GOOPS?


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
   (with-style ((fill-color *road-junction/color*))
     (rotate-in-place pi/4
                      (ref junction 'pos)
                      (fill (regular-polygon (ref junction 'pos)
                                             4
                                             *road-junction/size*))))))


(define (draw-road-segment segment)
  (let* ((v-beg (ref segment 'junction 'beg 'pos))
         (v-end (ref segment 'junction 'end 'pos))
         (v-to-edge (vec2* (get-v-ortho segment)
                           (/2 (get-width segment))))
         (p-1 (vec2+ v-beg v-to-edge))
         (p-2 (vec2- v-beg v-to-edge))
         (p-3 (vec2- v-end v-to-edge))
         (p-4 (vec2+ v-end v-to-edge))
         (road-painter (fill (polyline p-1 p-2 p-3 p-4 p-1))))

    (define (draw-road-lane lane)
      (let* ((direction (ref lane 'direction))
             (v-lane-offset (get-offset lane))
             (v-segment (vec2- v-end v-beg))
             (v-arrow-pos
              (vec2+/many v-beg
                          (vec2* v-segment 1/2)
                          v-lane-offset))
             (v-lane
              (vec2* v-segment (match-direction lane +1 -1)))
             (arrow-painter
              ;; `rotate' rotates clockwise (?!), triangle starts
              ;; pointing upwards
              (rotate-in-place (* -1 (- (angle-of v-lane) pi/2))
                               v-arrow-pos
                               (fill (regular-polygon v-arrow-pos 3
                                                      *road-lane/arrow-size*))))
             (line-painter
              (stroke (line (vec2+ v-beg v-lane-offset)
                            (vec2+ v-end v-lane-offset)))))
        (with-style ((stroke-color *road-lane/color*)
                     (fill-color *road-lane/color*))
          (superimpose line-painter arrow-painter))))

    (let* ((lane-painters (map draw-road-lane (get-lanes segment))))
      (with-canvas (apply superimpose road-painter (reverse lane-painters))))))

(define (draw-actor actor)
  (with-canvas (with-style ((fill-color *actor/color*))
                 (fill (circle (get-pos actor) *actor/size*)))))

(define (draw-world world)
  (for-each draw-road-segment (get-road-segments world))
  (for-each draw-road-junction (get-road-junctions world))
  (for-each draw-actor (get-actors world)))
