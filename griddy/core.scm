(define-module (griddy core)
  #:duplicates (merge-generics)
  #:use-module ((srfi srfi-1) #:select (fold first last))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (pipe)
  #:use-module (chickadee math vector)
  #:use-module (chickadee math bezier)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core actor)
  #:export (<location/off-road>
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
            connect-all!
            connect-by-rank!
            get-actors
            get-lanes
            get-length
            get-midpoint
            get-offset
            get-outgoing-lanes
            get-junction-lanes
            get-pos
            get-radius
            get-road-junctions
            get-road-lanes
            get-road-segments
            get-segment-lane
            get-segment-lanes
            get-vec
            get-ortho-vec
            get-tangent-vec
            get-width
            link!
            off-road->on-road
            on-road->off-road))

(util:extend-primitives!)
(math:extend-primitives!)
(define make-hash-table (@ (srfi srfi-69) make-hash-table))

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
  (junction #:init-keyword #:junction)
  curve)

(define-method (initialize (self <road-lane/junction>) initargs)
  (define (get-junction lane lane-type)
    (ref lane
         'segment
         'junction
         (match `(,lane-type ,(ref lane 'direction))
           (('in  'forw) 'end)
           (('in  'back) 'beg)
           (('out 'forw) 'beg)
           (('out 'back) 'end))))

  (let ((in-lane  (get-keyword #:in-lane initargs))
        (out-lane (get-keyword #:out-lane initargs)))
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
      (set! (ref self 'junction) junction)
      (set! (ref self 'curve) curve)))
  (next-method))

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

(define-method (get-pos (lane <road-lane/segment>) (beg-or-end <symbol>))
  (+ (get-pos (ref lane 'segment)
              (match-direction lane beg-or-end (flip beg-or-end)))
     (get-offset lane)))

(define-method (get-pos (lane <road-lane/junction>) (beg-or-end <symbol>))
  ((match beg-or-end
     ('beg bezier-curve-p0)
     ('end bezier-curve-p3))
   (ref lane 'curve)))

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

(define-method (get-pos (segment <road-segment>) (beg-or-end <symbol>))
  (let* ((junction (ref segment 'junction beg-or-end))
         (offset   (* (match beg-or-end
                        ('beg +1)
                        ('end -1))
                      (get-radius junction)
                      (get-tangent-vec segment))))
    (+ (ref junction 'pos) offset)))

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

(define-method (get-midpoint (straight-thing <static>))
  (+ (get-pos straight-thing 'beg)
     (* 1/2 (get-vec straight-thing))))

(define-method (get-midpoint (lane <road-lane/junction>))
  (bezier-curve-point-at (ref lane 'curve) 1/2))

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
(define-method (get-lanes (junction <road-junction>))
  "get all junction lanes for a junction"
  (hash-table-keys (ref junction 'lane-map 'outputs)))

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
  (let* ((junction-lane
          (make <road-lane/junction>
            #:in-lane  in-lane
            #:out-lane out-lane))
         (lane-maps
          (ref junction-lane 'junction 'lane-map)))
    (insert! (ref lane-maps 'inputs in-lane) junction-lane)
    (set! (ref lane-maps 'outputs junction-lane)
          out-lane)
    (add! world junction-lane)))

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
