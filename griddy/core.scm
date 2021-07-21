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
            get-pos-x
            get-pos-y
            get-road-junctions
            get-road-lanes
            get-road-segments
            length-of
            link!
            next-step
            pop-step!
            segment
            set-route))

;; classes ---------------------------------------------------------------------
(define-class <static> ())

(define <vec2> (class-of (vec2 0 0)))

(define-class <road-junction> (<static>)
  pos ;; vec2
  (pos-x
   #:getter get-pos-x
   #:allocation #:virtual
   #:slot-ref (lambda (self)
                (vec2-x (slot-ref self 'pos)))
   #:slot-set! (lambda (self val)
                 (set-vec2-x! (slot-ref self 'pos) val)))
  (pos-y
   #:getter get-pos-y
   #:allocation #:virtual
   #:slot-ref (lambda (self)
                (vec2-y (slot-ref self 'pos)))
   #:slot-set! (lambda (self val)
                 (set-vec2-y! (slot-ref self 'pos) val)))
  (segments
   #:init-thunk list))

(define-method (initialize (self <road-junction>) initargs)
  (slot-set! self 'pos (vec2 (get-keyword #:x initargs)
                             (get-keyword #:y initargs)))
  (next-method))

(define-method (get-road-lanes (junction <road-junction>))
  (fold (lambda (acc segment)
          (append acc (get segment 'road-lanes)))
        (list)))

(define (is-sink? lane junction)
  (or (and (eq? (get lane 'segment 'start-junction) junction)
           (eq? (get lane 'direction) 'forw))
      (and (eq? (get lane 'segment 'stop-junction) junction)
           (eq? (get lane 'direction) 'back))))

(define-method (get-sinks (junction <road-junction>))
  (filter is-sink? (get-road-lanes junction)))

(define-method (get-sources (junction <road-junction>))
  (filter (compose not is-sink?) (get-road-lanes junction)))

(define-class <road-lane> (<static>)
  segment
  (direction  ;; 'forw or 'back, relative to segment
   #:init-keyword #:direction)
  (actors
   ;; or maybe just use a list and lean on (chickadee math grid)
   #:init-form (make-bbtree
                >
                ;; (lambda (actor-1 actor-2)
                ;;   (> (get actor-1 'location 'pos-param)
                ;;      (get actor-2 'location 'pos-param)))
                )))

(define-class <road-segment> (<static>)
  start-junction
  stop-junction
  (lanes
   #:init-thunk list))

(define-method (length-of (segment <road-segment>))
  (l2 (get segment 'start-junction 'pos)
      (get segment 'stop-junction 'pos)))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane)
  (pos-param ;; 0..1
   #:init-value 0.0
   #:init-keyword #:pos-param))

(define-method (get-pos-x (loc <location>))
  (let* ((road-segment (get loc 'road-lane 'segment))
         (x1 (get road-segment 'start-junction 'pos-x))
         (x2 (get road-segment 'stop-junction 'pos-x)))
    (+ x1 (* (get loc 'pos-param) (- x2 x1)))))

(define-method (get-pos-y (loc <location>))
  (let* ((road-segment (get loc 'road-lane 'segment))
         (y1 (get road-segment 'start-junction 'pos-y))
         (y2 (get road-segment 'stop-junction 'pos-y)))
    (+ y1 (* (get loc 'pos-param) (- y2 y1)))))

(define-class <route> ()
  (steps
   #:init-thunk list
   #:init-keyword #:steps))

(define-method (pop-step! (route <route>))
  (slot-set! route 'steps (cdr (slot-ref route 'steps))))

(define-method (next-step (route <route>))
  (if (null? (get route 'steps))
      '()
      (car (get route 'steps))))

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed
   #:init-value 25) ;; units / second
  (route
   #:init-form (make <route>)
   #:setter set-route))

(define-method (get-pos-x (actor <actor>))
  (get-pos-x (get actor 'location)))

(define-method (get-pos-y (actor <actor>))
  (get-pos-y (get actor 'location)))

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

