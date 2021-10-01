(define-module (griddy draw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (chickadee math)
  #:use-module (chickadee math bezier)
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
  (define (draw-lane lane)
    (let* ((curve (ref lane 'curve)))
      (with-style ((stroke-color *road-lane/color*))
        (stroke (path (move-to   (bezier-curve-p0 curve))
                      (bezier-to (bezier-curve-p1 curve)
                                 (bezier-curve-p2 curve)
                                 (bezier-curve-p3 curve)))))))
  (let* ((junction-painter
          (fill (regular-polygon (ref junction 'pos)
                                 4
                                 ;; why 3/2??
                                 ;; doesn't seem to be exact anyway
                                 (* 3/2 (get-radius junction)))))

         (junction-painter
          (rotate-in-place pi/4
                           (ref junction 'pos)
                           junction-painter))
         (junction-painter
          (with-style ((fill-color *road-junction/color*))
            junction-painter))

         (lane-painters (map draw-lane (get-lanes junction))))
    (with-canvas (apply superimpose junction-painter lane-painters))))


(define (draw-road-segment segment)
  (define (draw-lane lane)
    (let* ((lane-beg-pos  (get-pos lane 'beg))
           (lane-end-pos  (get-pos lane 'end))
           (lane-vec      (get-vec lane))
           (line-painter  (stroke (line lane-beg-pos lane-end-pos)))
           (arrow-pos     (vec2+ lane-beg-pos (vec2* lane-vec 1/2)))
           (arrow-painter (fill (regular-polygon arrow-pos
                                                 3
                                                 *road-lane/arrow-size*)))
           ;; `rotate' rotates clockwise, triangle initially points upward
           (arrow-painter (rotate-in-place (- pi/2  (angle-of lane-vec))
                                           arrow-pos
                                           arrow-painter)))

      (with-style ((stroke-color *road-lane/color*)
                   (fill-color *road-lane/color*))
        (superimpose line-painter arrow-painter))))

  (let* ((beg-pos       (get-pos segment 'beg))
         (end-pos       (get-pos segment 'end))
         (edge-offset   (vec2* (get-ortho-vec segment)
                               (/2 (get-width segment))))
         (p-1           (vec2+ beg-pos edge-offset))
         (p-2           (vec2- beg-pos edge-offset))
         (p-3           (vec2- end-pos edge-offset))
         (p-4           (vec2+ end-pos edge-offset))
         (road-painter  (with-style ((fill-color *road-segment/color*))
                          (fill (polyline p-1 p-2 p-3 p-4 p-1))))
         (lane-painters (map draw-lane (get-lanes segment))))

    (with-canvas (apply superimpose road-painter lane-painters))))

(define (draw-actor actor)
  (with-canvas (with-style ((fill-color *actor/color*))
                 (fill (circle (get-pos actor) *actor/size*)))))

(define (draw-world world)
  (for-each draw-road-junction (get-road-junctions world))
  (for-each draw-road-segment (get-road-segments world))
  (for-each draw-actor (get-actors world)))
