(define-module (griddy core)
  #:use-module ((srfi srfi-1) #:select (fold
                                        first
                                        last))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math bezier)
  #:use-module (griddy util)
  #:use-module (griddy constants)
  #:use-module (griddy math)
  #:duplicates (merge-generics)
  #:export (<actor>
            <location/off-road>
            <location/on-road>
            <location>
            <point-like>
            <road-junction>
            <road-lane>
            <road-lane/segment>
            <road-lane/junction>
            <road-segment>
            <route>
            <static>
            <world>
            add!
            agenda-pop!
            agenda-push!
            connect-all-lanes!
            get-actors
            get-incoming-lanes
            get-lanes
            get-length
            get-midpoint
            get-offset
            get-outgoing-lanes
            get-pos
            get-radius
            get-road-junctions
            get-road-lanes
            get-road-segments
            get-vec
            get-ortho-vec
            get-tangent-vec
            get-width
            link!
            off-road->on-road
            on-road->off-road
            route-pop!
            route-reset!))

;; shouldn't be necessary :/
(define-syntax-rule (set! args ...)
  ((@ (griddy util) set!) args ...))

;; classes ---------------------------------------------------------------------
(define-class <static> ())

(define <vec2> (class-of (vec2 0 0)))

(define-class <road-lane> (<static>)
  (actors
   #:init-thunk list))

(define-class <road-lane/segment> (<road-lane>)
  segment
  (direction  ;; 'forw or 'back, relative to segment
   #:init-keyword #:direction)
  rank) ;; 0..

(define-class <road-lane/junction> (<road-lane>)
  ;; junction
  (segment
   #:init-form `((beg . ())
                 (end . ())))
  curve)

;; might want to refine these at some point
;; but for now just pass through to segment
(define-method (get-length (lane <road-lane/segment>))
  (get-length (ref lane 'segment)))

(define-method (get-pos (lane <road-lane/segment>) (which <symbol>))
  (vec2+ (get-pos (ref lane 'segment)
                  (match-direction lane which (flip which)))
         (get-offset lane)))

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

         (v-segment-edge  (vec2* v-ortho
                                 (* *road-lane/width*
                                    (get-lane-count segment)
                                    -1/2)))
         (v-lane-edge     (vec2+ v-segment-edge
                                 (vec2* v-ortho
                                        (* *road-lane/width*
                                           lane-count-from-edge))))
         (v-lane-center   (vec2+ v-lane-edge
                                 (vec2* v-ortho (* *road-lane/width*
                                                   1/2)))))
    v-lane-center))

(define make-hash-table (@ (srfi srfi-69) make-hash-table))

(define-class <road-junction> (<static>)
  pos ;; vec2
  (segments
   #:init-thunk list)
  (lane-map
   #:init-form `((inputs  . ,(make-hash-table eq?))
                 (outputs . ,(make-hash-table eq?)))))

(define-method (initialize (self <road-junction>) initargs)
  (set! (ref self 'pos) (vec2 (get-keyword #:x initargs)
                              (get-keyword #:y initargs)))
  (next-method))

(define-method (get-radius (junction <road-junction>))
  "<air-quote>radius</air-quote>"
  (let* ((max-segment-lane-count
          (apply max (map get-lane-count (ref junction 'segments))))
         (wiggle-factor
          (+ 1 (/ *road-segment/wiggle-room-%* 100))))
    (* wiggle-factor
       1/2
       max-segment-lane-count
       *road-lane/width*)))

(define-method (get-lanes (junction <road-junction>))
  (hash-table-keys (ref junction 'lane-map 'outputs)))

(define-method (get-segment-lanes (junction <road-junction>))
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
          (get-segment-lanes junction)))

(define-method (get-outgoing-lanes (junction <road-junction>))
  (filter (cut outgoing? <> junction)
          (get-segment-lanes junction)))

(define-class <road-segment> (<static>)
  (actors ;; off-road only, otherwise they belong to lanes
   #:init-thunk list)
  (junction
   #:init-form `((beg . ())
                 (end . ())))
  ;; lane lists are ascending by rank
  ;; higher rank means further from center/median
  (lanes
   #:init-form '((forw . ())
                 (back . ())))
  (lane-count
   #:init-form '((forw . 0)
                 (back . 0))))

(define-method (initialize (self <road-segment>) initargs)
  (next-method)
  (define alist-slots '(junction lanes lane-count))
  (define (make-mutable! slot)
    (set! (ref self slot) (copy-tree (ref self slot))))
  (for-each make-mutable! alist-slots))

(define-method (get-vec (segment <road-segment>))
  (vec2- (get-pos segment 'end)
         (get-pos segment 'beg)))

(define-method (get-vec (lane <road-lane/segment>))
  (vec2- (get-pos lane 'end)
         (get-pos lane 'beg)))

(define-method (get-tangent-vec (segment <road-segment>))
  ; can't use `get-vec' because recursive loop
  (vec2-normalize (vec2- (ref segment 'junction 'end 'pos)
                         (ref segment 'junction 'beg 'pos))))

(define-method (get-tangent-vec (lane <road-lane/segment>))
  (get-tangent-vec (ref lane 'segment)))

(define-method (get-ortho-vec (segment <road-segment>))
  (vec2-rotate (get-tangent-vec segment) pi/2))

(define-method (get-pos (segment <road-segment>) (which <symbol>))
  (let* ((junction (ref segment 'junction which))
         (offset   (vec2* (get-tangent-vec segment)
                          (* (match which
                               ('beg +1)
                               ('end -1))
                             (get-radius junction)))))
    (vec2+ (ref junction 'pos) offset)))

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

(define-method (add-lane-set-rank! (segment <road-segment>) (lane <road-lane/segment>))
  (let* ((direction (ref lane 'direction))
         (rank      (ref segment 'lane-count direction)))
    (set! (ref lane 'rank) rank)
    (set! (ref segment 'lane-count direction) (+ 1 rank))
    (extend! (ref segment 'lanes direction) lane)))

(define-method (get-outer-lane (segment <road-segment>) (direction <symbol>))
  (match `(,direction
           ,(ref segment 'lane-count 'back)
           ,(ref segment 'lane-count 'forw))
    ((_     0 0) (throw 'road-has-no-lanes))
    (('back 0 _) (first (get-lanes segment 'forw)))
    (('forw _ 0) (last (get-lanes segment 'back)))
    (('forw _ _) (last (get-lanes segment 'forw)))
    (('back _ _) (last (get-lanes segment 'back)))))

(define-method (get-width (segment <road-segment>))
  (* (+ 1 (/ *road-segment/wiggle-room-%* 100))
     *road-lane/width*
     (get-lane-count segment)))

(define-method (get-length (segment <road-segment>))
  (vec2-magnitude (get-vec segment)))

(define-method (get-midpoint (segment <road-segment>))
  (vec2+ (get-pos segment 'beg)
         (vec2* (get-vec segment) 1/2)))

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

(define-method (get-pos (loc <location/off-road>))
  (let* ((segment  (ref loc 'road-segment))
         (v-beg    (get-pos segment 'beg))
         (v-offset (vec2* (get-ortho-vec segment)  ;; magnitude is arbitrary
                          (* (match (ref loc 'road-side-direction)
                               ('forw +1)
                               ('back -1))
                             1/2
                             (get-width (ref loc 'road-segment))
                             (+ 1 (* 2 (/ *road-segment/wiggle-room-%* 100))))))
         (pos-param (ref loc 'pos-param)))
    (vec2+/many v-beg  v-offset (vec2* (get-vec segment) pos-param))))

(define-method (get-pos (loc <location/on-road>))
  (let* ((lane      (ref loc 'road-lane))
         (v-beg     (get-pos lane 'beg))
         (pos-param (ref loc 'pos-param)))
    (vec2+ v-beg (vec2* (get-vec lane) pos-param))))

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

(define-method (agenda-push! (actor <actor>) item)
  (extend! (ref actor 'agenda) item))

(define-method (agenda-pop! (actor <actor>))
  (let ((current-agenda (ref actor 'agenda)))
    (set! (ref actor 'agenda) (cdr current-agenda))
    (car current-agenda)))

(define-method (route-pop! (actor <actor>))
  (set! (ref actor 'route) (cdr (ref actor 'route))))

(define-method (route-reset! (actor <actor>))
  (set! (ref actor 'route) 'none))

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
(define-method (get-junction-lanes (lane <road-lane/segment>))
  )

(define-method (get-segment-lane (lane <road-lane/junction>))
  )

(define-method (connect! (in-lane <road-lane/segment>)
                         (out-lane <road-lane/segment>))

  (define (get-junction lane lane-type)
    (define which
      (match `(,lane-type ,(ref lane 'direction))
        (('in  'forw) 'end)
        (('in  'back) 'beg)
        (('out 'forw) 'beg)
        (('out 'back) 'end)))
    (ref lane 'segment 'junction which))

  (if (neq? (get-junction in-lane  'in)
            (get-junction out-lane 'out))
      (throw 'lanes-do-not-meet))
  (let* ((junction      (get-junction in-lane 'in))
         (junction-lane (make <road-lane/junction>))
         (delta         (* (get-radius junction)
                           (/ *road-segment/wiggle-room-%* 100)))
         (p0            (get-pos in-lane  'end))
         (p3            (get-pos out-lane 'beg))
         (p1            (vec2+ p0 (vec2* (get-tangent-vec in-lane)  delta)))
         (p2            (vec2- p0 (vec2* (get-tangent-vec out-lane) delta)))
         (curve         (make-bezier-curve p0 p1 p2 p3)))

    (set! (ref junction-lane 'curve)
          curve)

    ;; insert! can't do defaults atm :(
    (hash-table-update!/default (ref junction 'lane-map 'inputs)
                                in-lane
                                (cut cons junction-lane <>)
                                '())
    (set! (ref junction 'lane-map 'outputs junction-lane)
          out-lane)))

(define-method (connect-all-lanes! (junction <road-junction>))
  (let* ((in-lanes  (get-incoming-lanes junction))
         (out-lanes (get-outgoing-lanes junction)))
    (for-each
     (lambda (in-lane)
       (for-each
        (lambda (out-lane)
          (connect! in-lane out-lane))
        (filter (cut neq? in-lane <>) out-lanes)))
     in-lanes)))

;; -----------------------------------------------------------------------------
(define-method (link! (lane <road-lane/segment>) (segment <road-segment>))
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

(define-method (link! (actor <actor>) (loc <location/off-road>))
  (set! (ref actor 'location) loc)
  (insert! (ref loc 'road-segment 'actors) actor))

(define-method (link! (actor <actor>) (loc <location/on-road>))
  (set! (ref actor 'location) loc)
  (insert! (ref loc 'road-lane 'actors) actor))

(define-method (add! (world <world>) (static-item <static>))
  (insert! (ref world 'static-items) static-item))

(define-method (add! (world <world>) (segment <road-segment>))
  (cond
   ((or (null? (ref segment 'junction 'beg))
        (null? (ref segment 'junction 'end)))
    (throw 'unlinked-road-segment))
   ((and (null? (ref segment 'lanes 'forw))
         (null? (ref segment 'lanes 'back)))
    (throw 'road-segment-has-no-lanes))
   (else
    (next-method))))
