(define-module (griddy core dimension)
  #:duplicates (merge-generics)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  ;; #:use-module (ice-9 match)
  #:use-module (pipe)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math vector)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core static)
  #:use-module (griddy core position)
  #:use-module (griddy core location)
  #:export (get-length
            get-width
            get-radius))

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
