(define-module (gritty core)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pfds bbtrees)
  #:use-module (pfds queues)
  #:use-module (gritty math)
  #:export (<point-like>
            <road-junction>
            <road-lane>
            <road-segment>
            <location>
            <actor>
            <world>
            actors
            size-x
            size-y
            link!
            add!
            pos-x
            pos-y
            road-junctions
            road-segments
            start-junction
            stop-junction
            forward-lanes
            backward-lanes
            location
            road-lane
            segment))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <point-like> ()
  pos-x
  pos-y)

(define-class <road-junction> (<point-like>)
  (pos-x
   #:init-keyword #:x
   #:getter pos-x)
  (pos-y
   #:init-keyword #:y
   #:getter pos-y)
  (segments
   #:init-form '()
   #:getter segments))

(define-class <road-lane> ()
  (segment
   #:getter segment)
  (actors
   #:getter actors
   #:init-form (make-bbtree
                (lambda (a1 a2)
                  (> (pos-param (location a1))
                     (pos-param (location a2)))))))

(define-class <road-segment> ()
  (start-junction #:getter start-junction)
  (stop-junction #:getter stop-junction)
  (forward-lanes
   #:accessor forward-lanes
   #:init-keyword #:forward-lanes
   #:init-form (list (make <road-lane>)))
  (backward-lanes
   #:accessor backward-lanes
   #:init-keyword #:backward-lanes
   #:init-form (list (make <road-lane>))))

(define-method (length-of (rs <road-segment>))
  (l2 (pos-x (start-junction rs))
      (pos-y (start-junction rs))
      (pos-x (stop-junction rs))
      (pos-y (stop-junction rs))))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane
   #:getter road-lane)
  (pos-param ;; 0..1
   #:init-form 0.0
   #:init-keyword #:pos-param
   #:getter pos-param))

(define-method (pos-x (loc <location>))
  (let* ((road-segment (segment (road-lane loc)))
         (x1 (pos-x (start-junction road-segment)))
         (x2 (pos-x (stop-junction road-segment))))
    (+ x1 (* (pos-param loc) (- x2 x1)))))

(define-method (pos-y (loc <location>))
  (let* ((road-segment (segment (road-lane loc)))
         (y1 (pos-y (start-junction road-segment)))
         (y2 (pos-y (stop-junction road-segment))))
    (+ y1 (* (pos-param loc) (- y2 y1)))))

(define-class <actor> (<point-like>)
  (location
   #:init-keyword #:location
   #:getter location)
  (max-speed
   #:init-keyword #:max-speed
   #:getter max-speed)
  (route
   #:init-form (make-queue)
   #:accessor route))

(define-method (pos-x (actor <actor>))
  (pos-x (location actor)))

(define-method (pos-y (actor <actor>))
  (pos-y (location actor)))

(define-class <world> ()
  (size-x
   #:init-value 0
   #:getter size-x)
  (size-y
   #:init-value 0
   #:getter size-y)
  (road-junctions
   #:init-form '()
   #:getter road-junctions)
  (road-segments
   #:init-form '()
   #:getter road-segments))

(define-method (actors (world <world>))
  (define (segment-into-lanes segment lanes)
    (append lanes
            (forward-lanes segment)
            (backward-lanes segment)))

  (define (lane-into-actors lane actors-)
    (append actors-
            (map cdr (bbtree->alist (actors lane)))))

  (fold lane-into-actors '()
        (fold segment-into-lanes '()
              (road-segments world))))

(define-method (link! (lane <road-lane>) (segment <road-segment>) direction)
  (if (slot-bound? lane 'segment)
      (throw 'lane-already-linked))
  (slot-set! lane 'segment segment)
  (case direction
    ((forward) (slot-push! segment 'forward-lanes lane))
    ((backward) (slot-push! segment 'backward-lanes lane))))

(define-method (link! (junction-1 <road-junction>)
                      (segment <road-segment>)
                      (junction-2 <road-junction>))
  (slot-push! junction-1 'segments segment)
  (slot-push! junction-2 'segments segment)
  (slot-set! segment 'start-junction junction-1)
  (slot-set! segment 'stop-junction junction-2))

(define-method (link! (actor <actor>) (lane <road-lane>) pos-param)
  (let ((loc (make <location>
               #:road-lane lane
               #:pos-param pos-param)))
    (slot-set! actor 'location loc)
    (slot-set! lane
               'actors
               (bbtree-set (actors lane) pos-param actor))))

(define-method (add! (world <world>) (junction <road-junction>))
  (slot-push! world 'road-junctions junction)
  (if (> (pos-x junction) (size-x world))
      (slot-set! world 'size-x (pos-x junction)))
  (if (> (pos-y junction) (size-y world))
      (slot-set! world 'size-y (pos-y junction))))

(define-method (add! (world <world>) (segment <road-segment>))
  (unless (and (slot-bound? segment 'start-junction)
               (slot-bound? segment 'stop-junction))
    (throw 'unlinked-road-segment))
  (if (and (null? (forward-lanes segment))
           (null? (backward-lanes segment)))
      (throw 'road-segment-has-no-lanes))
  (slot-push! world 'road-segments segment))
