(define-module (griddy core)
  #:use-module ((srfi srfi-1) #:select (fold
                                        first
                                        last))
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee math vector)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:duplicates (warn merge-generics)
  #:export (
            <actor>
            <location-off-road>
            <location-on-road>
            <location>
            <point-like>
            <road-junction>
            <road-lane>
            <road-segment>
            <route>
            <static>
            <world>
            add!
            agenda-append!
            agenda-pop!
            get-actors
            get-incoming-lanes
            get-lanes
            get-length
            get-midpoint
            get-offset
            get-outgoing-lanes
            get-pos
            get-road-junctions
            get-road-lanes
            get-road-segments
            get-v
            get-v-ortho
            get-v-tangent
            get-width
            link!
            match-direction
            off-road->on-road
            on-road->off-road
            route-pop!
            segment
            set-route!
            ))

(define *core/road-lane/width* 20)
(define *core/road-segment/wiggle-room-%* 10)

(define-syntax-rule (match-direction lane if-forw if-back)
  (case (get lane 'direction)
    ((forw) if-forw)
    ((back) if-back)))

;; classes ---------------------------------------------------------------------
(define-class <static> ())

(define <vec2> (class-of (vec2 0 0)))

(define-class <road-lane> (<static>)
  segment
  (direction  ;; 'forw or 'back, relative to segment
   #:init-keyword #:direction)
  rank ;; 0..
  (actors
   #:init-thunk list))

(define-method (get-offset (lane <road-lane>))
  (let* ((segment
          (get lane 'segment))
         (lane-count-from-edge
          (match (get lane 'direction)
            ;; swap back/forw for uk-style
            ('back (- (get-lane-count segment 'back)
                      (get lane 'rank)
                      1))
            ('forw (+ (get-lane-count segment 'back)
                      (get lane 'rank)))))

         (v-ortho         (get-v-ortho segment))

         (v-segment-edge  (vec2* v-ortho
                                 (* *core/road-lane/width*
                                    (get-lane-count segment)
                                    -1/2)))
         (v-lane-edge     (vec2+ v-segment-edge
                                 (vec2* v-ortho
                                        (* *core/road-lane/width*
                                           lane-count-from-edge))))
         (v-lane-center   (vec2+ v-lane-edge
                                 (vec2* v-ortho (* *core/road-lane/width*
                                                   1/2)))))
    v-lane-center))


(define-class <road-junction> (<static>)
  pos ;; vec2
  (segments
   #:init-thunk list))

(define-method (initialize (self <road-junction>) initargs)
  (slot-set! self 'pos (vec2 (get-keyword #:x initargs)
                             (get-keyword #:y initargs)))
  (next-method))

(define-method (get-lanes (junction <road-junction>))
  (fold (lambda (segment lanes)
          (append lanes (get-lanes segment)))
        (list)
        (get junction 'segments)))

(define (outgoing? lane junction)
  (or (and (eq? (get lane 'segment 'start-junction) junction)
           (eq? (get lane 'direction) 'forw))
      (and (eq? (get lane 'segment 'stop-junction) junction)
           (eq? (get lane 'direction) 'back))))

(define-method (get-incoming-lanes (junction <road-junction>))
  (filter (negate (cut outgoing? <> junction))
          (get-lanes junction)))

(define-method (get-outgoing-lanes (junction <road-junction>))
  (filter (cut outgoing? <> junction)
          (get-lanes junction)))

(define-class <road-segment> (<static>)
  (actors ;; off-road only, otherwise they belong to lanes
   #:init-thunk list)
  start-junction
  stop-junction
  ;; lane lists are ascending by rank
  ;; higher rank means further from center/median
  (forw-lanes
   #:init-thunk list)
  (back-lanes
   #:init-thunk list)
  (forw-lane-count
   #:init-value 0)
  (back-lane-count
   #:init-value 0))

(define-method (get-v (segment <road-segment>))
  (vec2- (get segment 'stop-junction 'pos)
         (get segment 'start-junction 'pos)))

(define-method (get-v-tangent (segment <road-segment>))
  (vec2-normalize (get-v segment)))

(define-method (get-v-ortho (segment <road-segment>))
  (vec2-rotate (get-v-tangent segment) pi/2))

(define-method (get-lane-count (segment <road-segment>))
  (+ (get segment 'forw-lane-count)
     (get segment 'back-lane-count)))

(define-method (get-lane-count (segment <road-segment>) (direction <symbol>))
  (get segment (match direction
                 ('forw 'forw-lane-count)
                 ('back 'back-lane-count))))

(define-method (get-lanes (segment <road-segment>))
  (append (get segment 'forw-lanes)
          (get segment 'back-lanes)))

(define-method (get-lanes (segment <road-segment>) (direction <symbol>))
  (get segment (match direction
                 ('forw 'forw-lanes)
                 ('back 'back-lanes))))

(define-method (add-lane-set-rank! (segment <road-segment>) (lane <road-lane>))
  (let* ((lane-slot  (match-direction lane 'forw-lanes      'back-lanes))
         (count-slot (match-direction lane 'forw-lane-count 'back-lane-count))
         (rank       (get segment count-slot)))
    (slot-set! lane 'rank rank)
    (slot-set! segment count-slot (+ 1 rank))
    (slot-append! segment lane-slot lane)))

(define-method (get-outer-lane (segment <road-segment>) (direction <symbol>))
  (match `(,direction
           ,(get segment 'back-lane-count)
           ,(get segment 'forw-lane-count))
    (((or 'forw 'back) 0 0)
     (throw 'road-has-no-lanes))
    (('back 0 _)
     (first (get-lanes segment 'forw)))
    (('forw _ 0)
     (last (get-lanes segment 'back)))
    (('forw _ _)
     (last (get-lanes segment 'forw)))
    (('back _ _)
     (last (get-lanes segment 'back)))))

(define-method (get-width (segment <road-segment>))
  (* (+ 1 (/ *core/road-segment/wiggle-room-%* 100))
     *core/road-lane/width*
     (get-lane-count segment)))

(define-method (get-length (segment <road-segment>))
  (l2 (get segment 'start-junction 'pos)
      (get segment 'stop-junction 'pos)))

(define-method (get-midpoint (segment <road-segment>))
  (vec2+ (get segment 'start-junction 'pos)
         (vec2* (get-v segment) 1/2)))

(define-class <location> ()
  (pos-param ;; 0..1
   #:init-value 0.0
   #:init-keyword #:pos-param))

(define-class <location-on-road> (<location>)
  (road-lane
   #:init-keyword #:road-lane))

(define-class <location-off-road> (<location>)
  (road-segment
   #:init-keyword #:road-segment)
  (road-side-direction
   #:init-keyword #:road-side-direction))

(define (get-pos-helper v-start v-stop v-offset pos-param)
  (vec2+/many v-start
              (vec2* (vec2- v-stop v-start) pos-param)
              v-offset))

(define-method (get-pos (loc <location-off-road>))
  (let* ((segment (get loc 'road-segment))
         (v-start  (get segment 'start-junction 'pos))
         (v-stop   (get segment 'stop-junction  'pos))
         (v-offset (vec2* (get-v-ortho segment)
                          (* 1/2
                             (get-width (get loc 'road-segment))
                             (+ 1 (/ *core/road-segment/wiggle-room-%* 100)))))
         (pos-param (get loc 'pos-param)))
    (get-pos-helper v-start v-stop v-offset pos-param)))

(define-method (get-pos (loc <location-on-road>))
  (let ((v-start  (get loc 'road-lane 'segment 'start-junction 'pos))
        (v-stop   (get loc 'road-lane 'segment 'stop-junction  'pos))
        (v-offset (get-offset (get loc 'road-lane)))
        (pos-param (get loc 'pos-param)))
    (get-pos-helper v-start v-stop v-offset pos-param)))

(define-method (on-road->off-road (loc <location-on-road>))
  (make <location-off-road>
    #:pos-param (get loc 'pos-param)
    #:road-segment (get loc 'road-lane 'road-segment)
    #:road-side-direction (get loc 'road-lane 'direction)))

(define-method (off-road->on-road (loc <location-off-road>))
  (make <location-on-road>
    #:pos-param (get loc 'pos-param)
    #:road-lane (get-outer-lane (get loc 'road-segment)
                                (get loc 'road-side-direction))))

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed
   #:init-value 25) ;; units / second
  (route ;; 'none or list
         ;; '() means end of route
   #:init-form 'none
   #:setter set-route!)
  (agenda
   #:init-thunk list))

(define-method (get-pos (actor <actor>))
  (get-pos (get actor 'location)))

(define-method (agenda-append! (actor <actor>) item)
  (slot-append! actor 'agenda item))

(define-method (agenda-pop! (actor <actor>))
  (let ((current-agenda (get actor 'agenda)))
    (slot-set! actor 'agenda (cdr current-agenda))
    (car current-agenda)))

(define-method (route-pop! (actor <actor>))
  (slot-set! actor 'route (cdr (slot-ref actor 'route))))

(define-class <world> ()
  (static-items ;; roads, etc
   #:init-thunk list))

(define-method (get-road-lanes (world <world>))
  (filter (cut is-a? <> <road-lane>)
          (get world 'static-items)))

(define-method (get-road-junctions (world <world>))
  (filter (cut is-a? <> <road-junction>)
          (get world 'static-items)))

(define-method (get-road-segments (world <world>))
  (filter (cut is-a? <> <road-segment>)
          (get world 'static-items)))

(define-method (get-actors (world <world>))
  (define (into-actors container actors)
    (append actors (get container 'actors)))
  (append (fold into-actors (list) (get-road-lanes world))
          (fold into-actors (list) (get-road-segments world))))


;; -----------------------------------------------------------------------------
(define-method (link! (lane <road-lane>) (segment <road-segment>))
  (if (slot-bound? lane 'segment)
      (throw 'lane-already-linked lane segment))
  (slot-set! lane 'segment segment)
  (add-lane-set-rank! segment lane))

(define-method (link! (junction-1 <road-junction>)
                      (segment    <road-segment>)
                      (junction-2 <road-junction>))
  (slot-add! junction-1 'segments segment)
  (slot-add! junction-2 'segments segment)
  (slot-set! segment 'start-junction junction-1)
  (slot-set! segment 'stop-junction  junction-2))

(define-method (link! (actor <actor>) (loc <location-off-road>))
  (slot-set! actor 'location loc)
  (slot-add! (get loc 'road-segment) 'actors actor))

(define-method (link! (actor <actor>) (loc <location-on-road>))
  (slot-set! actor 'location loc)
  (slot-add! (get loc 'road-lane) 'actors actor))

(define-method (add! (world <world>) (static-item <static>))
  (slot-add! world 'static-items static-item))

(define-method (add! (world <world>) (segment <road-segment>))
  (unless (and (slot-bound? segment 'start-junction)
               (slot-bound? segment 'stop-junction))
    (throw 'unlinked-road-segment))
  (when (and (null? (get segment 'forw-lanes))
             (null? (get segment 'back-lanes)))
    (throw 'road-segment-has-no-lanes))
  (next-method))

