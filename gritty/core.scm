(define-module (gritty core)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
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
            get
            get-actors
            get-pos-x
            get-pos-y
            link!
            add!
            segment))

(define (get obj . slots)
  ;; apparently `match' doesn't support tail patterns?
  ;; (match slots
  ;;   ((slots)
  ;;    (slot-ref obj slot))
  ;;   ((rest ... last)
  ;;    (slot-ref (get obj rest ...) last)))
  (define (but-last l) (drop-right l 1))
  (cond
   ((= 1 (length slots))
    (slot-ref obj (car slots)))
   (else
    (slot-ref (apply get (cons obj (but-last slots)))
              (last slots)))))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <road-junction> ()
  (pos-x
   #:init-keyword #:x
   #:getter get-pos-x)
  (pos-y
   #:init-keyword #:y
   #:getter get-pos-y)
  (segments
   #:init-form '()))

(define-class <road-lane> ()
  segment
  (actors
   #:init-form (make-bbtree
                (lambda (actor-1 actor-2)
                  (> (get actor-1 'location 'pos-param)
                     (get actor-2 'location 'pos-param))))))

(define-class <road-segment> ()
  start-junction
  stop-junction
  (forward-lanes
   #:init-form '())
  (backward-lanes
   #:init-form '()))

(define-method (length-of (segment <road-segment>))
  (l2 (get segment 'start-junction 'pos-x)
      (get segment 'start-junction 'pos-y)
      (get segment 'stop-junction 'pos-x)
      (get segment 'stop-junction 'pos-y)))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane
   #:getter get-road-lane)
  (pos-param ;; 0..1
   #:init-form 0.0
   #:init-keyword #:pos-param
   #:getter get-pos-param))

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

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed)
  (route
   #:init-form (make-queue)))

(define-method (get-pos-x (actor <actor>))
  (get-pos-x (get actor 'location)))

(define-method (get-pos-y (actor <actor>))
  (get-pos-y (get actor 'location)))

(define-class <world> ()
  (size-x
   #:init-value 0)
  (size-y
   #:init-value 0)
  (road-junctions
   #:init-form '())
  (road-segments
   #:init-form '()))

(define-method (get-actors (world <world>))
  (define (segment-into-lanes segment lanes)
    (append lanes
            (get segment 'forward-lanes)
            (get segment 'backward-lanes)))
  (define (lane-into-actors lane actors)
    (append actors
            (map cdr (bbtree->alist (get lane 'actors)))))
  (fold lane-into-actors '()
        (fold segment-into-lanes '()
              (get world 'road-segments))))

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
    (slot-set! actor 'location
               loc)
    (slot-set! lane 'actors
               (bbtree-set (slot-ref lane 'actors) pos-param actor))))

(define-method (add! (world <world>) (junction <road-junction>))
  (slot-push! world 'road-junctions junction)
  (if (> (get junction 'pos-x) (get world 'size-x))
      (slot-set! world 'size-x (get junction 'pos-x )))
  (if (> (get junction 'pos-y) (get world 'size-y))
      (slot-set! world 'size-y (get junction 'pos-y ))))

(define-method (add! (world <world>) (segment <road-segment>))
  (unless (and (slot-bound? segment 'start-junction)
               (slot-bound? segment 'stop-junction))
    (throw 'unlinked-road-segment))
  (if (and (null? (get segment 'forward-lanes))
           (null? (get segment 'backward-lanes)))
      (throw 'road-segment-has-no-lanes))
  (slot-push! world 'road-segments segment))
