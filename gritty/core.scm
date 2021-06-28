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
            get-actors
            get-size-x
            get-size-y
            link!
            add!
            get-pos-x
            get-pos-y
            get-road-junctions
            get-road-segments
            get-start-junction
            get-stop-junction
            get-forward-lanes
            get-backward-lanes
            get-location
            get-road-lane
            segment))


;; doesn't work
(define-syntax get
  (syntax-rules ()
    ((get obj slots ... slot) ;; is this valid r7rs? guile?
     (slot-ref (get obj slots ...) 'slot))
    ((get obj slot)
     (slot-ref obj 'slot))))


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
   #:getter get-pos-x)
  (pos-y
   #:init-keyword #:y
   #:getter get-pos-y)
  (segments
   #:init-form '()
   #:getter get-segments))

(define-class <road-lane> ()
  (segment
   #:getter get-segment)
  (actors
   #:getter get-actors
   #:init-form (make-bbtree
                (lambda (a1 a2)
                  (> (get-pos-param (get-location a1))
                     (get-pos-param (get-location a2)))))))

(define-class <road-segment> ()
  (start-junction
   #:getter get-start-junction)
  (stop-junction
   #:getter get-stop-junction)
  (forward-lanes
   #:getter get-forward-lanes
   #:init-keyword #:forward-lanes
   #:init-form (list (make <road-lane>)))
  (backward-lanes
   #:getter get-backward-lanes
   #:init-keyword #:backward-lanes
   #:init-form (list (make <road-lane>))))

(define-method (length-of (segment <road-segment>))
  (l2 (get-pos-x (get-start-junction segment))
      (get-pos-y (get-start-junction segment))
      (get-pos-x (get-stop-junction segment))
      (get-pos-y (get-stop-junction segment))))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane
   #:getter get-road-lane)
  (pos-param ;; 0..1
   #:init-form 0.0
   #:init-keyword #:pos-param
   #:getter get-pos-param))

(define-method (get-pos-x (loc <location>))
  (let* ((road-segment (get-segment (get-road-lane loc)))
         (x1 (get-pos-x (get-start-junction road-segment)))
         (x2 (get-pos-x (get-stop-junction road-segment))))
    (+ x1 (* (get-pos-param loc) (- x2 x1)))))

(define-method (get-pos-y (loc <location>))
  (let* ((road-segment (get-segment (get-road-lane loc)))
         (y1 (get-pos-y (get-start-junction road-segment)))
         (y2 (get-pos-y (get-stop-junction road-segment))))
    (+ y1 (* (get-pos-param loc) (- y2 y1)))))

(define-class <actor> (<point-like>)
  (location
   #:init-keyword #:location
   #:getter get-location)
  (max-speed
   #:init-keyword #:max-speed
   #:getter get-max-speed)
  (route
   #:init-form (make-queue)
   #:accessor route))

(define-method (get-pos-x (actor <actor>))
  (get-pos-x (get-location actor)))

(define-method (get-pos-y (actor <actor>))
  (get-pos-y (get-location actor)))

(define-class <world> ()
  (size-x
   #:init-value 0
   #:getter get-size-x)
  (size-y
   #:init-value 0
   #:getter get-size-y)
  (road-junctions
   #:init-form '()
   #:getter get-road-junctions)
  (road-segments
   #:init-form '()
   #:getter get-road-segments))

(define-method (get-actors (world <world>))
  (define (segment-into-lanes segment lanes)
    (append lanes
            (get-forward-lanes segment)
            (get-backward-lanes segment)))
  (define (lane-into-actors lane actors)
    (append actors
            (map cdr (bbtree->alist (get-actors lane)))))
  (fold lane-into-actors '()
        (fold segment-into-lanes '()
              (get-road-segments world))))

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
               (bbtree-set (get-actors lane) pos-param actor))))

(define-method (add! (world <world>) (junction <road-junction>))
  (slot-push! world 'road-junctions junction)
  (if (> (get-pos-x junction) (get-size-x world))
      (slot-set! world 'size-x (get-pos-x junction)))
  (if (> (get-pos-y junction) (get-size-y world))
      (slot-set! world 'size-y (get-pos-y junction))))

(define-method (add! (world <world>) (segment <road-segment>))
  (unless (and (slot-bound? segment 'start-junction)
               (slot-bound? segment 'stop-junction))
    (throw 'unlinked-road-segment))
  (if (and (null? (get-forward-lanes segment))
           (null? (get-backward-lanes segment)))
      (throw 'road-segment-has-no-lanes))
  (slot-push! world 'road-segments segment))
