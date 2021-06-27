(define-module (gritty core)
  #:use-module (oop goops)
  #:use-module (pfds bbtrees)
  #:use-module (pfds queues)
  #:use-module (gritty math)
  #:export (<1d>
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

(define-class <1d> ()
  pos-x
  pos-y)

(define-class <road-junction> (<1d>)
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

;; can't figure out how to import/export this properly
;; so using a `link!' instead
;; (define-method (initialize (self <road-segment>) args)
;;   (next-method)
;;   (define (set-segment! lane)
;;     (slot-set! lane 'segment self))
;;   (for-each set-segment! (forward-lanes self))
;;   (for-each set-segment! (backward-lanes self)))

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

(define-method (pos-x (l <location>))
  (let* ((road-segment (segment (road-lane l)))
         (x1 (pos-x (start-junction road-segment)))
         (x2 (pos-x (stop-junction road-segment))))
    (+ x1 (* (pos-param l) (- x2 x1)))))

(define-method (pos-y (l <location>))
  (let* ((road-segment (segment (road-lane l)))
         (y1 (pos-y (start-junction road-segment)))
         (y2 (pos-y (stop-junction road-segment))))
    (+ y1 (* (pos-param l) (- y2 y1)))))

(define-class <actor> (<1d>)
  (location
   #:init-keyword #:location
   #:getter location)
  (max-speed
   #:init-keyword #:max-speed
   #:getter max-speed)
  (route
   #:init-form (make-queue)
   #:accessor route)
  (pos-x
   #:getter pos-x
   #:allocation #:virtual
   #:slot-ref (lambda (self) (pos-x (location self)))
   #:slot-set! (lambda (self val) (raise 'read-only)))
  (pos-y
   #:getter pos-y
   #:allocation #:virtual
   #:slot-ref (lambda (self) (pos-y (location self)))
   #:slot-set! (lambda (self val) (raise 'read-only))))

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
  (road-lanes
   ;;; todo: do we really need this? just for consistency...
   #:init-form '()
   #:getter road-lanes)
  (actors
   #:init-form '()
   #:getter actors))


(define-method (link! (l <road-lane>)
                      (s <road-segment>)
                      direction)
  (if (slot-bound? l 'segment)
      (throw 'lane-already-linked))
  (slot-set! l 'segment s)
  (case direction
    ((forward) (slot-push! s 'forward-lanes l))
    ((backward) (slot-push! s 'backward-lanes l))))

(define-method (link! (j1 <road-junction>)
                      (s <road-segment>)
                      (j2 <road-junction>))
  (slot-push! j1 'segments s)
  (slot-push! j2 'segments s)
  (slot-set! s 'start-junction j1)
  (slot-set! s 'stop-junction j2))

(define-method (link! (a <actor>) (l <road-lane>) pos-param)
  (let ((loc (make <location>
               #:road-lane l
               #:pos-param pos-param)))
    (slot-set! a 'location loc)
    (slot-set! l
               'actors
               (bbtree-set (actors l) pos-param a))))

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
  (if (and (null? (forward-lanes s))
           (null? (backward-lanes s)))
      (throw 'road-segment-has-no-lanes))
  (slot-push! w 'road-segments s))

(define-method (add! (w <world>) (l <road-lane>))
  (unless (slot-bound? l 'segment)
    (throw 'road-lane-has-no-segment))
  (slot-push! w 'road-lanes l))

(define-method (add! (w <world>) (a <actor>))
  (unless (slot-bound? a 'location)
    (throw 'actor-missing-location))
  (slot-push! w 'actors a))


