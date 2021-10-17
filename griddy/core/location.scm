(define-module (griddy core location)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (first last))
  #:use-module (griddy util)
  #:use-module (griddy core road)
  #:duplicates (merge-generics)
  #:export (<location/off-road>
            <location/on-road>
            <location>
            off-road->on-road
            on-road->off-road))

(util:extend-primitives!)

(define-method (get-outer-lane (segment <road-segment>) (direction <symbol>))
  (match `(,direction
           ,(ref segment 'lane-count 'back)
           ,(ref segment 'lane-count 'forw))
    ((_     0 0) (throw 'road-has-no-lanes))
    (('back 0 _) (first (get-lanes segment 'forw)))
    (('forw _ 0) (last (get-lanes segment 'back)))
    (('forw _ _) (last (get-lanes segment 'forw)))
    (('back _ _) (last (get-lanes segment 'back)))))

(define-class <location> ()
  (pos-param ;; 0..1
   #:init-value 0.0
   #:init-keyword #:pos-param))

(define-class <location/on-road> (<location>)
  (road-lane
   #:init-keyword #:road-lane))

(define-class <location/off-road> (<location>)
  (road-segment
   #:init-keyword #:road-segment)
  (road-side-direction
   #:init-keyword #:road-side-direction))

(define-method (on-road->off-road (loc <location/on-road>))
  (make <location/off-road>
    #:pos-param           (match-direction (ref loc 'road-lane)
                            (ref loc 'pos-param)
                            (- 1 (ref loc 'pos-param)))
    #:road-segment        (ref loc 'road-lane 'segment)
    #:road-side-direction (ref loc 'road-lane 'direction)))

(define-method (off-road->on-road (loc <location/off-road>))
  (let* ((road-lane (get-outer-lane (ref loc 'road-segment)
                                    (ref loc 'road-side-direction)))
         (pos-param (match-direction road-lane
                      (ref loc 'pos-param)
                      (- 1 (ref loc 'pos-param)))))
    (make <location/on-road>
      #:pos-param pos-param
      #:road-lane road-lane)))
