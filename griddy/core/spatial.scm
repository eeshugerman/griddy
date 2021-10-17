(define-module (griddy core spatial)
  #:duplicates (merge-generics)
  #:use-module ((srfi srfi-1) #:select (fold last first))
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (pipe)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math vector)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core road)
  #:use-module (griddy core actor)
  #:use-module (griddy core location)
  #:export (get-length
            get-width
            get-radius
            get-midpoint
            get-pos
            get-vec
            get-tangent-vec
            get-ortho-vec))

(util:extend-primitives!)
(math:extend-primitives!)

(define-method (get-length (segment <road-segment>))
  (vec2-magnitude (get-vec segment)))

(define-method (get-length (lane <road-lane/segment>))
  (get-length (ref lane 'segment)))

(define-method (get-length (lane <road-lane/junction>))
  "approximate"
  (let* ((n         *road-lane/approx-pts*)
         (1/n       (recip n))
         (t->pt     (cut bezier-curve-point-at (ref lane 'curve) <>))
         (pts-low   (map t->pt (iota n 0   1/n)))
         (pts-high  (map t->pt (iota n 1/n 1/n))))

    (fold (lambda (pt-low pt-high acc)
            (+ acc (vec2-magnitude (- pt-high pt-low))))
          0
          pts-low
          pts-high)))

(define-method (get-width (segment <road-segment>))
  (* (+ 1 (/ *road-segment/wiggle-room-%* 100))
     *road-lane/width*
     (get-lane-count segment)))

(define-method (get-radius (junction <road-junction>))
  "<air-quote>radius</air-quote>"
  (let* ((max-segment-lane-count
          (->> (ref junction 'segments)
               (map get-lane-count)
               (apply max)))
         (wiggle-factor
          (-> *road-segment/wiggle-room-%*
              (/ 100)
              (+ 1))))
    (* wiggle-factor
       1/2
       max-segment-lane-count
       *road-lane/width*)))

(define-method (get-midpoint (straight-thing <road-component>))
  (+ (get-pos straight-thing 'beg)
     (* 1/2 (get-vec straight-thing))))

(define-method (get-midpoint (lane <road-lane/junction>))
  (bezier-curve-point-at (ref lane 'curve) 1/2))

(define-method (get-offset (lane <road-lane/segment>))
  (let* ((segment         (ref lane 'segment))
         (lane-count-from-edge
          (match (ref lane 'direction)
            ;; swap back/forw for uk-style
            ('back (- (get-lane-count segment 'back)
                      (ref lane 'rank)
                      1))
            ('forw (+ (get-lane-count segment 'back)
                      (ref lane 'rank)))))

         (v-ortho         (get-ortho-vec segment))

         (v-segment-edge  (* -1/2
                             *road-lane/width*
                             (get-lane-count segment)
                             v-ortho))

         (v-lane-edge     (+ v-segment-edge
                             (* lane-count-from-edge
                                *road-lane/width*
                                v-ortho)))

         (v-lane-center   (+ v-lane-edge
                             (* 1/2
                                *road-lane/width*
                                v-ortho))))
    v-lane-center))

(define-method (get-pos (lane <road-lane/segment>) (beg-or-end <symbol>))
  (+ (get-pos (ref lane 'segment)
              (match-direction lane beg-or-end (flip beg-or-end)))
     (get-offset lane)))

(define-method (get-pos (lane <road-lane/junction>) (beg-or-end <symbol>))
  ((match beg-or-end
     ('beg bezier-curve-p0)
     ('end bezier-curve-p3))
   (ref lane 'curve)))

(define-method (get-pos (segment <road-segment>) (beg-or-end <symbol>))
  (let* ((junction (ref segment 'junction beg-or-end))
         (offset   (* (match beg-or-end
                        ('beg +1)
                        ('end -1))
                      (get-radius junction)
                      (get-tangent-vec segment))))
    (+ (ref junction 'pos) offset)))


(define-method (get-pos (loc <location/off-road>))
  (let* ((segment  (ref loc 'road-segment))
         (v-beg    (get-pos segment 'beg))
         (v-offset (* (match (ref loc 'road-side-direction)
                        ('forw +1)
                        ('back -1))
                      1/2
                      (get-width (ref loc 'road-segment))
                      (+ 1 (* 2 (/ *road-segment/wiggle-room-%* 100)))
                      (get-ortho-vec segment)))  ;; magnitude is arbitrary
         (pos-param (ref loc 'pos-param)))
    (+ v-beg  v-offset (* pos-param (get-vec segment)))))

(define-method (get-pos (loc <location/on-road>))
  (let* ((lane      (ref loc 'road-lane))
         (pos-param (ref loc 'pos-param)))
    (cond
     ((is-a? lane <road-lane/segment>)
      (+ (get-pos lane 'beg)
         (* pos-param (get-vec lane))))
     ((is-a? lane <road-lane/junction>)
      (bezier-curve-point-at (ref lane 'curve) pos-param)))))

(define-method (get-pos (actor <actor>))
  (get-pos (ref actor 'location)))


(define-method (get-vec (segment <road-segment>))
  (- (get-pos segment 'end)
     (get-pos segment 'beg)))

(define-method (get-vec (lane <road-lane/segment>))
  (- (get-pos lane 'end)
     (get-pos lane 'beg)))

(define-method (get-tangent-vec (segment <road-segment>))
                                        ; can't use `get-vec' because recursive loop
  (vec2-normalize (- (ref segment 'junction 'end 'pos)
                     (ref segment 'junction 'beg 'pos))))

(define-method (get-tangent-vec (lane <road-lane/segment>))
  (* (match-direction lane +1 -1)
     (get-tangent-vec (ref lane 'segment))))

(define-method (get-ortho-vec (segment <road-segment>))
  (vec2-rotate (get-tangent-vec segment) pi/2))
