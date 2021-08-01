(define-module (griddy core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (pfds bbtrees)
  #:use-module (chickadee math vector)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:export (<actor>
            <location>
            <point-like>
            <road-junction>
            <road-lane>
            <road-segment>
            <route>
            <static>
            <world>
            add!
            get-actors
            get-offset
            get-incoming-lanes
            get-length
            get-midpoint
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
            agenda-pop!
            agenda-append!
            segment
            set-route!))

(define *core/road-lane/width* 20)
(define *core/road-segment/wiggle-room-%* 10)

;; classes ---------------------------------------------------------------------
(define-class <static> ())

(define <vec2> (class-of (vec2 0 0)))

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
          (append lanes (get segment 'lanes)))
        (list)
        (get junction 'segments)))

(define (is-sink? lane junction)
  (or (and (eq? (get lane 'segment 'start-junction) junction)
           (eq? (get lane 'direction) 'forw))
      (and (eq? (get lane 'segment 'stop-junction) junction)
           (eq? (get lane 'direction) 'back))))

(define-method (get-incoming-lanes (junction <road-junction>))
  (filter (cut is-sink? <> junction)
          (get-lanes junction)))

(define-method (get-outgoing-lanes (junction <road-junction>))
  (filter (negate (cut is-sink? <> junction))
          (get-lanes junction)))

(define-class <road-segment> (<static>)
  start-junction
  stop-junction
  (lanes
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
  (match direction
    ('forw (get segment 'forw-lane-count))
    ('back (get segment 'back-lane-count))))

(define-method (bump-lane-count! (segment <road-segment>) (direction <symbol>))
  (let* ((slot (match direction
                 ('forw 'forw-lane-count)
                 ('back 'back-lane-count)))
         (val (slot-ref segment slot))
         (new-val (+ 1 val)))
    (slot-set! segment slot new-val)))

(define-method (get-width (segment <road-segment>))
  (* (+ 1 (/ *core/road-segment/wiggle-room-%* 100))
     *core/road-lane/width*
     (get-lane-count segment)))

(define-method (get-length (segment <road-segment>))
  (l2 (get segment 'start-junction 'pos)
      (get segment 'stop-junction 'pos)))

(define-method (get-midpoint (segment <road-segment>))
  (vec2+ (get segment 'start-junction)
         (vec2* (get-v segment) 1/2)))

(define-class <road-lane> (<static>)
  segment
  (direction  ;; 'forw or 'back, relative to segment
   #:init-keyword #:direction)
  rank ;; 0..
  (actors
   ;; or maybe just use a list and lean on (chickadee math grid)
   #:init-form (make-bbtree >)))

(define-syntax-rule (match-direction lane if-forw if-back)
  (case (get lane 'direction)
    ((forw) if-forw)
    ((back) if-back)))

(define-method (get-offset (lane <road-lane>))
  (let* ((segment (get lane 'segment))
         (lane-count-from-edge
          (match (get lane 'direction)
            ('forw (- (get-lane-count segment 'forw)
                      (get lane 'rank)
                      1))
            ('back (+ (get-lane-count segment 'forw)
                      (get lane 'rank)))))
         (v-ortho
          (get-v-ortho segment))
         (v-segment-edge
          (vec2* v-ortho (* *core/road-lane/width*
                            (get-lane-count segment)
                            -1/2)))
         (v-lane-edge
          (vec2+ v-segment-edge
                 (vec2* v-ortho (* *core/road-lane/width*
                                   lane-count-from-edge))))
         (v-lane-center
          (vec2+ v-lane-edge
                 (vec2* v-ortho (* *core/road-lane/width*
                                   1/2)))))
    v-lane-center))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane)
  (pos-param ;; 0..1
   #:init-value 0.0
   #:init-keyword #:pos-param))

(define-method (get-pos (loc <location>))
  (let* ((v-start (get loc 'road-lane 'segment 'start-junction 'pos))
         (v-stop (get loc 'road-lane 'segment 'stop-junction 'pos))
         (v-segment (vec2- v-stop v-start)))
    (vec2+/many v-start
                (vec2* v-segment (get loc 'pos-param))
                (get-offset (get loc 'road-lane)))))

(define-class <route> ()
  (steps
   #:init-thunk list
   #:init-keyword #:steps))

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed
   #:init-value 25) ;; units / second
  (route
   #:init-form (make <route>)
   #:setter set-route!)
  (agenda
   #:init-thunk list
   #:setter set-agenda!))

(define-method (get-pos (actor <actor>))
  (get-pos (get actor 'location)))

(define-method (agenda-append! (actor <actor>) item)
  (slot-append! actor 'agenda item))

(define-method (agenda-pop! (actor <actor>))
  (let ((current-agenda (get actor 'agenda)))
    (slot-set! actor 'agenda (cdr current-agenda))
    (car current-agenda)))

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
  (define (lane-into-actors lane actors)
    (append actors
            (map cdr (bbtree->alist (get lane 'actors)))))
  (fold lane-into-actors (list) (get-road-lanes world)))


;; -----------------------------------------------------------------------------
(define-method (link! (lane <road-lane>) (segment <road-segment>))
  (if (slot-bound? lane 'segment)
      (throw 'lane-already-linked lane segment))

  (let ((direction (get lane 'direction)))
    (slot-set! lane 'rank (get-lane-count segment direction))
    (bump-lane-count! segment direction))
  (slot-set! lane 'segment segment)
  (slot-add! segment 'lanes lane))

(define-method (link! (junction-1 <road-junction>)
                      (segment <road-segment>)
                      (junction-2 <road-junction>))
  (slot-add! junction-1 'segments segment)
  (slot-add! junction-2 'segments segment)
  (slot-set! segment 'start-junction junction-1)
  (slot-set! segment 'stop-junction junction-2))

(define-method (link! (actor <actor>) (loc <location>))
  (slot-set! actor 'location loc)
  (slot-add! (get loc 'road-lane)
             'actors
             (cons (get loc 'pos-param) actor)))

(define-method (add! (world <world>) (static-item <static>))
  (slot-add! world 'static-items static-item))

(define-method (add! (world <world>) (segment <road-segment>))
  (unless (and (slot-bound? segment 'start-junction)
               (slot-bound? segment 'stop-junction))
    (throw 'unlinked-road-segment))
  (if (null? (get segment 'lanes))
      (throw 'road-segment-has-no-lanes))
  (next-method))

