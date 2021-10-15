(define-module (griddy draw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (pipe)
  #:use-module (chickadee math)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math matrix)
  #:use-module (chickadee graphics path)
  #:use-module (griddy core)
  #:use-module (griddy constants)
  #:use-module (griddy math)
  #:use-module (griddy util)
  #:export (make-skeleton-canvas
            make-actors-canvas))

;; TODO: Possible to use GOOPS?


(define (angle-of vec)
  (atan (vec2-y vec) (vec2-x vec)))

(define (make-road-junction-painter junction)
  (define (make-lane-painter lane)
    (let* ((curve (ref lane 'curve)))
      (with-style ((stroke-color *road-lane/color*))
        (stroke (path (move-to   (bezier-curve-p0 curve))
                      (bezier-to (bezier-curve-p1 curve)
                                 (bezier-curve-p2 curve)
                                 (bezier-curve-p3 curve)))))))

  (let* ((size              (* 3/2 (get-radius junction))) ;; not exact
         (junction-painter  (->> (regular-polygon origin 4 size)
                                 (fill)
                                 (rotate pi/4)
                                 (translate (ref junction 'pos))))
         (junction-painter (with-style ((fill-color *road-junction/color*))
                             junction-painter))
         (lane-painters    (map make-lane-painter (get-lanes junction))))
    (apply superimpose junction-painter lane-painters)))


(define (make-road-segment-painter segment)
  (define (make-lane-painter lane)
    (let* ((lane-beg-pos  (get-pos lane 'beg))
           (lane-end-pos  (get-pos lane 'end))
           (lane-vec      (get-vec lane))
           (line-painter  (stroke (line lane-beg-pos lane-end-pos)))
           (arrow-pos     (vec2+ lane-beg-pos (vec2* lane-vec 1/2)))
           (arrow-painter (->> (regular-polygon origin 3 *road-lane/arrow-size*)
                               (fill)
                               ;; `rotate' rotates clockwise, triangle
                               ;; initially points upward
                               (rotate (- pi/2  (angle-of lane-vec)))
                               (translate arrow-pos))))
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
         (lane-painters (map make-lane-painter (get-lanes segment))))

    (apply superimpose road-painter lane-painters)))

(define (make-actor-painter actor)
  (with-style ((fill-color *actor/color*))
    (fill (circle (get-pos actor) *actor/size*))))

(define (make-skeleton-canvas world)
  (make-canvas
   (apply superimpose
          (append
           (map make-road-junction-painter (get-road-junctions world))
           (map make-road-segment-painter (get-road-segments world))))))

(define (make-actors-canvas world)
  (make-canvas
   (apply superimpose
          (map make-actor-painter (get-actors world)))))
