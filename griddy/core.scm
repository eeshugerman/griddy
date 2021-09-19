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
  #:duplicates (merge-generics)
  #:export (<actor>
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
            push-agenda-item!
            pop-agenda-item!
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
            pop-route-step!
            segment))

;; shouldn't be necessary :/
(define-syntax-rule (set! args ...)
  ((@ (griddy util) set!) args ...))

(define *core/road-lane/width* 10)
(define *core/road-segment/wiggle-room-%* 5)

(define-syntax-rule (match-direction lane if-forw if-back)
  (case (ref lane 'direction)
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
          (ref lane 'segment))
         (lane-count-from-edge
          (match (ref lane 'direction)
            ;; swap back/forw for uk-style
            ('back (- (get-lane-count segment 'back)
                      (ref lane 'rank)
                      1))
            ('forw (+ (get-lane-count segment 'back)
                      (ref lane 'rank)))))

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
  (set! (ref self 'pos) (vec2 (get-keyword #:x initargs)
                              (get-keyword #:y initargs)))
  (next-method))

(define-method (get-lanes (junction <road-junction>))
  (fold (lambda (segment lanes)
          (append lanes (get-lanes segment)))
        (list)
        (ref junction 'segments)))

(define (outgoing? lane junction)
  (or (and (eq? (ref lane 'segment 'junction 'beg) junction)
           (eq? (ref lane 'direction) 'forw))
      (and (eq? (ref lane 'segment 'junction 'end) junction)
           (eq? (ref lane 'direction) 'back))))

(define-method (get-incoming-lanes (junction <road-junction>))
  (filter (negate (cut outgoing? <> junction))
          (get-lanes junction)))

(define-method (get-outgoing-lanes (junction <road-junction>))
  (filter (cut outgoing? <> junction)
          (get-lanes junction)))

(define-class <road-segment> (<static>)
  (actors ;; off-road only, otherwise they belong to lanes
   #:init-thunk list)
  (junction
   #:init-form `((beg . undefined)
                 (end . undefined)))
  ;; lane lists are ascending by rank
  ;; higher rank means further from center/median
  (lanes
   #:init-form '((forw . ())
                 (back . ())))
  (lane-count
   #:init-form '((forw . 0)
                 (back . 0))))

(define-method (initialize (self <road-segment>) initargs)
  ;; https://lists.gnu.org/archive/html/bug-guile/2018-09/msg00032.html
  (define alist-slots '(junction lanes lane-count))
  (define (make-mutable! slot)
    (set! (ref self slot) (copy-tree (ref self slot))))
  (next-method)
  (for-each make-mutable! alist-slots))

(define-method (get-v (segment <road-segment>))
  (vec2- (ref segment 'junction 'end 'pos)
         (ref segment 'junction 'beg 'pos)))

(define-method (get-v-tangent (segment <road-segment>))
  (vec2-normalize (get-v segment)))

(define-method (get-v-ortho (segment <road-segment>))
  (vec2-rotate (get-v-tangent segment) pi/2))

(define-method (get-lane-count (segment <road-segment>))
  (+ (ref segment 'lane-count 'forw)
     (ref segment 'lane-count 'back)))

(define-method (get-lane-count (segment <road-segment>) (direction <symbol>))
  (ref segment 'lane-count direction))

(define-method (get-lanes (segment <road-segment>))
  (append (ref segment 'lanes 'forw)
          (ref segment 'lanes 'back)))

(define-method (get-lanes (segment <road-segment>) (direction <symbol>))
  (ref segment 'lanes direction))

(define-method (add-lane-set-rank! (segment <road-segment>) (lane <road-lane>))
  (let* ((direction (ref lane 'direction))
         (rank      (ref segment 'lane-count direction)))
    (set! (ref lane 'rank) rank)
    (set! (ref segment 'lane-count direction) (+ 1 rank))
    (extend! (ref segment 'lanes direction) lane)))

(define-method (get-outer-lane (segment <road-segment>) (direction <symbol>))
  (match `(,direction
           ,(ref segment 'lane-count 'back)
           ,(ref segment 'lane-count 'forw))
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
  (l2 (ref segment 'junction 'beg 'pos)
      (ref segment 'junction 'end 'pos)))

(define-method (get-midpoint (segment <road-segment>))
  (vec2+ (ref segment 'junction 'beg 'pos)
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

(define (get-pos-helper v-beg v-end v-offset pos-param)
  (vec2+/many v-beg
              (vec2* (vec2- v-end v-beg) pos-param)
              v-offset))

(define-method (get-pos (loc <location-off-road>))
  (let* ((segment  (ref loc 'road-segment))
         (v-beg    (ref segment 'junction 'beg 'pos))
         (v-end    (ref segment 'junction 'end  'pos))
         (v-offset (vec2* (get-v-ortho segment)  ;; magnitude is arbitrary
                          (* (match (ref loc 'road-side-direction)
                               ('forw +1)
                               ('back -1))
                             1/2
                             (get-width (ref loc 'road-segment))
                             (+ 1 (* 2 (/ *core/road-segment/wiggle-room-%* 100))))))
         (pos-param (ref loc 'pos-param)))
    (get-pos-helper v-beg v-end v-offset pos-param)))

(define-method (get-pos (loc <location-on-road>))
  (let ((v-beg     (ref loc 'road-lane 'segment 'junction 'beg 'pos))
        (v-end     (ref loc 'road-lane 'segment 'junction 'end 'pos))
        (v-offset  (get-offset (ref loc 'road-lane)))
        (pos-param (ref loc 'pos-param)))
    (get-pos-helper v-beg v-end v-offset pos-param)))

(define-method (on-road->off-road (loc <location-on-road>))
  (make <location-off-road>
    #:pos-param (ref loc 'pos-param)
    #:road-segment (ref loc 'road-lane 'segment)
    #:road-side-direction (ref loc 'road-lane 'direction)))

(define-method (off-road->on-road (loc <location-off-road>))
  (make <location-on-road>
    #:pos-param (ref loc 'pos-param)
    #:road-lane (get-outer-lane (ref loc 'road-segment)
                                (ref loc 'road-side-direction))))

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed
   #:init-value 25) ;; units / second
  (route ;; 'none or list
         ;; '() means end of route
   #:init-form 'none)
  (agenda
   #:init-thunk list))

(define-method (get-pos (actor <actor>))
  (get-pos (ref actor 'location)))

(define-method (push-agenda-item! (actor <actor>) item)
  (extend! (ref actor 'agenda) item))

(define-method (pop-agenda-item! (actor <actor>))
  (let ((current-agenda (ref actor 'agenda)))
    (set! (ref actor 'agenda) (cdr current-agenda))
    (car current-agenda)))

(define-method (pop-route-step! (actor <actor>))
  (set! (ref actor 'route) (cdr (slot-ref actor 'route))))

(define-class <world> ()
  (static-items ;; roads, etc
   #:init-thunk list))

(define-method (get-road-lanes (world <world>))
  (filter (cut is-a? <> <road-lane>)
          (ref world 'static-items)))

(define-method (get-road-junctions (world <world>))
  (filter (cut is-a? <> <road-junction>)
          (ref world 'static-items)))

(define-method (get-road-segments (world <world>))
  (filter (cut is-a? <> <road-segment>)
          (ref world 'static-items)))

(define-method (get-actors (world <world>))
  (define (into-actors container actors)
    (append actors (ref container 'actors)))
  (append (fold into-actors (list) (get-road-lanes world))
          (fold into-actors (list) (get-road-segments world))))


;; -----------------------------------------------------------------------------
(define-method (link! (lane <road-lane>) (segment <road-segment>))
  (if (slot-bound? lane 'segment)
      (throw 'lane-already-linked lane segment))
  (set! (ref lane 'segment) segment)
  (add-lane-set-rank! segment lane))

(define-method (link! (junction-1 <road-junction>)
                      (segment    <road-segment>)
                      (junction-2 <road-junction>))
  (insert! (ref junction-1 'segments) segment)
  (insert! (ref junction-2 'segments) segment)
  (set! (ref segment 'junction 'beg) junction-1)
  (set! (ref segment 'junction 'end) junction-2))

(define-method (link! (actor <actor>) (loc <location-off-road>))
  (set! (ref actor 'location) loc)
  (insert! (ref loc 'road-segment 'actors) actor))

(define-method (link! (actor <actor>) (loc <location-on-road>))
  (set! (ref actor 'location) loc)
  (insert! (ref loc 'road-lane 'actors) actor))

(define-method (add! (world <world>) (static-item <static>))
  (insert! (ref world 'static-items) static-item))

(define-method (add! (world <world>) (segment <road-segment>))
  (cond
   [(or (eq? 'undefined (ref segment 'junction 'beg))
        (eq? 'undefined (ref segment 'junction 'end)))
    (throw 'unlinked-road-segment)]
   [(and (null? (ref segment 'lanes 'forw))
         (null? (ref segment 'lanes 'back)))
    (throw 'road-segment-has-no-lanes)]
   [else
    (next-method)]))

