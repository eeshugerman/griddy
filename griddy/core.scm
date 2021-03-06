(define-module (griddy core)
  #:use-module ((srfi srfi-1) #:select (fold first last))
  #:use-module (srfi srfi-26)
  #:use-module ((srfi srfi-69) #:select (hash-table-keys))
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math bezier)
  #:use-module (pfds bbtrees)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core world)
  #:use-module (griddy core road)
  #:use-module (griddy core location)
  #:use-module (griddy core actor)
  #:use-module (griddy core spatial)
  #:duplicates (merge-generics)
  #:export (connect-all!
            connect-by-rank!
            get-junction-lanes
            get-segment-lane
            link!)
  #:re-export (
               ;; (griddy core world)
               <world>
               <static-item>
               add!
               get-actors
               get-static-items

               ;; (griddy core road)
               <road-component>
               <road-junction>
               <road-segment>
               <road-lane>
               <road-lane/segment>
               <road-lane/junction>
               get-lanes

               ;; (griddy core location)
               <location>
               <location/on-road>
               <location/off-road>
               off-road->on-road
               on-road->off-road

               ;; (griddy core spatial)
               get-pos
               get-midpoint
               get-vec
               get-ortho-vec
               get-tangent-vec
               get-length
               get-width
               get-radius

               ;; (griddy core actor)
               <actor>
               agenda-pop!
               agenda-push!
               route-pop!
               route-reset!))

(util:extend-primitives!)
(math:extend-primitives!)

;; -----------------------------------------------------------------------------
(define-method (get-direction (incoming-or-outgoing <symbol>)
                              (junction <road-junction>)
                              (segment <road-segment>))
  "get the direction (forw/back) of incoming lanes of `segment' w.r.t `junction'"
  (cond
   ((eq? junction (ref segment 'junction 'end))
    (match incoming-or-outgoing
      ('incoming 'forw)
      ('outgoing 'back)))
   ((eq? junction (ref segment 'junction 'beg))
    (match incoming-or-outgoing
      ('incoming 'back)
      ('outgoing 'forw)))
   (else
    (throw 'no-connection junction segment))))

(define-method (get-segment-lanes (incoming-or-outgoing <symbol>)
                                  (junction <road-junction>) )
  "get all outgoing or all incoming segment lanes for a junction"
  (fold (lambda (segment lanes)
          (let* ((direction (get-direction incoming-or-outgoing
                                           junction
                                           segment))
                 (new-lanes (get-lanes segment direction)))
            (append lanes new-lanes)))
        (list)
        (ref junction 'segments)))

(define-method (get-junction-lanes (lane <road-lane/segment>))
  "get the junction lanes that connect to _incoming_ segment lane `lane'"
  (ref/default lane
               'segment
               'junction (match-direction lane 'end 'beg)
               'lane-map 'inputs
               lane
               '()))

(define-method (get-segment-lane (lane <road-lane/junction>))
  "get the _outgoing_ segment lanes that connect to junciton lane `lane'"
  (ref lane
       'junction
       'lane-map 'outputs
       lane))

(define-method (connect! (in-lane <road-lane/segment>)
                         (out-lane <road-lane/segment>)
                         (world <world>))
  (let ((junction-lane (make <road-lane/junction>)))
    (link! in-lane junction-lane out-lane)
    (let ((lane-maps (ref junction-lane 'junction 'lane-map)))
      (insert! (ref lane-maps 'inputs in-lane) junction-lane)
      (set! (ref lane-maps 'outputs junction-lane)
            out-lane)
      (add! world junction-lane))))

(define-method (connect-all! (junction <road-junction>) (world <world>))
  (let* ((in-lanes  (get-segment-lanes 'incoming junction))
         (out-lanes (get-segment-lanes 'outgoing junction)))
    (for-each
     (lambda (in-lane)
       (for-each
        (lambda (out-lane)
          (connect! in-lane out-lane world))
        (filter (cut neq? in-lane <>) out-lanes)))
     in-lanes)))

(define-method (connect-by-rank! (junction <road-junction>) (world <world>))
  "not quite right, but better than `connect-all!'"
  (for-each
   (lambda (in-segment)
     (for-each
      (lambda (out-segment)
        ((@ (srfi srfi-1) for-each)
         (cut connect! <> <> world)
         (reverse (get-lanes in-segment
                             (get-direction 'incoming junction in-segment)))
         (reverse (get-lanes out-segment
                             (get-direction 'outgoing junction out-segment)))))
      (ref junction 'segments)))
   (ref junction 'segments)))

;; -----------------------------------------------------------------------------
(define-method (link! (lane <road-lane/segment>) (segment <road-segment>))
  (if (slot-bound? lane 'segment)
      (throw 'lane-already-linked lane segment))
  (set! (ref lane 'segment) segment)
  (let* ((direction (ref lane 'direction))
         (rank      (ref segment 'lane-count direction)))
    (set! (ref lane 'rank) rank)
    (set! (ref segment 'lane-count direction) (+ 1 rank))
    (extend! (ref segment 'lanes direction) lane)))

(define-method (link! (junction-1 <road-junction>)
                      (segment    <road-segment>)
                      (junction-2 <road-junction>))
  (insert! (ref junction-1 'segments) segment)
  (insert! (ref junction-2 'segments) segment)
  (set! (ref segment 'junction 'beg) junction-1)
  (set! (ref segment 'junction 'end) junction-2))

(define-method (link! (actor <actor>) (loc <location/off-road>))
  (set! (ref actor 'location) loc)
  (insert! (ref loc 'road-segment 'actors) actor))

(define-method (link! (actor <actor>) (loc <location/on-road>))
  (set! (ref actor 'location) loc)
  (set! (ref loc 'road-lane 'actors)
        (bbtree-set (ref loc 'road-lane 'actors)
                    (ref actor 'location 'pos-param)
                    actor)))

(define-method (link! (in-lane <road-lane/segment>)
                      (junction-lane <road-lane/junction>)
                      (out-lane <road-lane/segment>))
  (define (get-junction lane lane-type)
    (ref lane
         'segment
         'junction
         (match `(,lane-type ,(ref lane 'direction))
           (('in  'forw) 'end)
           (('in  'back) 'beg)
           (('out 'forw) 'beg)
           (('out 'back) 'end))))

  (unless (eq? (get-junction in-lane 'in)
               (get-junction out-lane 'out))
    (throw 'no-connection in-lane out-lane))
  (let* ((junction (get-junction in-lane 'in))
         (offset   (* 2
                      (get-radius junction)
                      (/ *road-segment/wiggle-room-%* 100)))
         (p0       (get-pos in-lane  'end))
         (p3       (get-pos out-lane 'beg))
         (p1       (+ p0 (* offset (get-tangent-vec in-lane))))
         (p2       (- p3 (* offset (get-tangent-vec out-lane))))
         (curve    (make-bezier-curve p0 p1 p2 p3)))
    (set! (ref junction-lane 'junction) junction)
    (set! (ref junction-lane 'curve) curve)))
