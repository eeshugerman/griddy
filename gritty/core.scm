(define-module (gritty core)
  #:use-module (oop goops)
  #:use-module (pfds bbtrees)
  #:use-module (pfds queues)
  #:export (<road-junction>
            <road-lane>
            <road-segment>
            <location>
            <actor>
            <world>
            link!
            add!))

(define (l2 v1 v2)
  (define (square x) (expt x 2))
  (let ((dx (- (car v1) (car v2)))
        (dy (- (cdr v1) (cdr v2))))
    (sqrt (+ (square dx) (square dy)))))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <road-junction> ()
  (position ;; (x . y)
   #:init-keyword #:position
   #:accessor position)
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
  (l2 (position (start-junction rs))
      (position (stop-junction rs))))

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
  (slot-push! w 'road-junctions j))

(define-method (add! (w <world>) (s <road-segment>))
  (unless (and (slot-bound? s 'start-junction)
               (slot-bound? s 'stop-junction))
    (throw 'unlinked-road-segment))
  (slot-push! w 'road-segments s))
