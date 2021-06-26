(define-module (gritty core)
  #:use-module (oop goops)
  #:use-module (pfds bbtrees)
  #:use-module (pfds queues)
  #:use-module (gritty math)
  #:export (<road-junction>
            <road-lane>
            <road-segment>
            <location>
            <actor>
            <world>
            size-x
            size-y
            link!
            add!
            pos-x
            pos-y
            road-junctions))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <road-junction> ()
  (pos-x
   #:init-keyword #:x
   #:accessor pos-x)
  (pos-y
   #:init-keyword #:y
   #:accessor pos-y)
  (segments
   #:init-form '()
   #:getter segments))

(define-class <road-lane> ()
  segment
  (actors
   #:accessor actors
   #:init-form (make-bbtree
                (lambda (a1 a2)
                  (> (lane-s (location a1))
                     (lane-s (location a1)))))))

(define-class <road-segment> ()
  (start-junction #:getter start-junction)
  (stop-junction #:getter stop-junction)
  (lanes
   #:getter lanes
   #:init-keyword #:lanes
   #:init-form `((frwd . ,(list (make <road-lane>)))
                  (bkwd . ,(list (make <road-lane>))))))

(define-method (initialize (self <road-segment>))
  (define (set-segment! lane)
    (slot-set! lane 'segment self))
  (for-each set-segment! (assoc-ref 'frwd (lanes self)))
  (for-each set-segment! (assoc-ref 'bkwd (lanes self))))

(define-method (length-of (rs <road-segment>))
  (l2 (pos-x (start-junction rs))
      (pos-y (start-junction rs))
      (pos-x (stop-junction rs))
      (pos-y (stop-junction rs))))

(define-class <location> ()
  (road-lane
   #:init-keyword #:road-lane
   #:accessor road-lane)
  (lane-s ;; 0..1
   #:init-form 0.0
   #:init-keyword #:lane-s
   #:accessor lane-s)
  (direction
   #:init-form 'forward
   #:init-keyword #:direction
   #:accessor direction)
  ;; (position
  ;;  #:allocation #:virtual)
  )

(define-class <actor> ()
  (location
   #:init-keyword #:location
   #:getter location)
  (max-speed
   #:init-keyword #:max-speed
   #:getter max-speed)
  (route
   #:init-form (make-queue)
   #:accessor route))

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
   #:getter road-segments)
  (actors
   #:init-form '()
   #:getter actors))

(define-method (link! (j1 <road-junction>)
                      (s <road-segment>)
                      (j2 <road-junction>))
  (slot-push! j1 'segments s)
  (slot-push! j2 'segments s)
  (slot-set! s 'start-junction j1)
  (slot-set! s 'stop-junction j2))

(define-method (add! (w <world>) (j <road-junction>))
  (slot-push! w 'road-junctions j)
  (if (> (pos-x j) (size-x w))
      (slot-set! w 'size-x (pos-x j)))
  (if (> (pos-y j) (size-y w))
      (slot-set! w 'size-y (pos-y j))))

(define-method (add! (w <world>) (s <road-segment>))
  (unless (and (slot-bound? s 'start-junction)
               (slot-bound? s 'stop-junction))
    (throw 'unlinked-road-segment))
  (slot-push! w 'road-segments s))
