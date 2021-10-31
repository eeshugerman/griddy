(define-module (griddy simulate)
  #:duplicates (merge-generics)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee graphics path)
  ;; #:use-module (chickadee)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy draw)
  #:use-module (griddy math)
  #:use-module (griddy core)
  #:use-module (griddy event-loop)
  #:use-module (griddy simulate route)
  #:export (simulate))

(util:extend-primitives!)

(define make-hash-table (@ (srfi srfi-69) make-hash-table))

(define (make-++ world world++)
  (define static-items-table (make-hash-table eq?))
  (for-each
   (cut hash-table-set! static-items-table <> <>)
   (ref world 'static-items)
   (ref world++ 'static-items))

  (define actors-table (make-hash-table eq?))

  (define-generic ++)

  (define-method (++ obj)
    obj)

  (define-method (++ (list' <list>))
    (map ++ list'))

  (define-method (++ (item <static-item>))
    (hash-table-ref static-items-table item))

  (define-method (++ (location <location/on-road>))
    (make <location/on-road>
      #:pos-param (++ (ref location 'pos-param))
      #:road-lane (++ (ref location 'road-lane))))

  (define-method (++ (location <location/off-road>))
    (make <location/off-road>
      #:pos-param (++ (ref location 'pos-param))
      #:road-segment (++ (ref location 'road-segment))
      #:road-side-direction (++ (ref location 'road-side-direction))))

  (define-method (++ (actor <actor>))
    (hash-table-ref
     actors-table
     actor
     (lambda ()
       (define new-actor (make <actor>))
       (define (copy-slot-if-bound! slot)
         (if (slot-bound? actor slot)
             (set! (ref new-actor slot) (++ (ref actor slot)))))
       (for-each copy-slot-if-bound!
                 '(max-speed agenda route))
       (hash-table-set! actors-table actor new-actor)
       new-actor)))

  ++)

(define-method (do-nothing$ (++ <generic>) (actor <actor>))
  (link! (++ actor)
         (++ (ref actor 'location))))

(define-method (sleep$ (++ <generic>) (actor <actor>) time)
  (if (= time 0)
    (agenda-pop! (++ actor))
    (set-car! (ref (++ actor) 'agenda) `(sleep-for ,(- time 1))))
  (do-nothing$ ++ actor))

(define-method (begin-route$ (++ <generic>) (actor <actor>) (dest <location/off-road>))
  (let* ((init-loc (off-road->on-road (ref actor 'location)))
         (dest-loc (off-road->on-road dest))
         (route    (find-route init-loc dest-loc)))
    (set! (ref (++ actor) 'route) (++ route))
    (link! (++ actor) (++ init-loc))))

(define-method (end-route$ (++ <generic>) (actor <actor>))
  (route-reset! (++ actor))
  (agenda-pop! (++ actor))
  (link! (++ actor)
         (++ (on-road->off-road (ref actor 'location)))))

(define-method (advance$ (++ <generic>) (actor <actor>))
  (let ((agenda-status
         (match (ref actor 'agenda)
           (()                     'nothing)
           ((agenda-step _ ...)    agenda-step)))

        (route-status
         (match (ref actor 'route)
           ('none   'none)
           ((_ ..1) 'some)
           (()      'done))))

    (match `(,agenda-status ,route-status)
      (('nothing          'none) (do-nothing$       ++ actor))
      ((('sleep-for time) 'none) (sleep$            ++ actor time))
      ((('travel-to dest) 'none) (begin-route$      ++ actor dest))
      ((('travel-to dest) 'some) (advance-on-route$ ++ actor))
      ((('travel-to dest) 'done) (end-route$        ++ actor))
      )))

(define world #f)
(define skeleton-canvas #f)

;; todo: use new `current-timestep' param?
(define sim-t0 0)

(define update-times '())
(define draw-times '())

(define (current-time)
  ((@ (srfi srfi-19) current-time) time-process))

(define <time> (class-of (current-time)))
(define-method (- (t0 <time>) (t1 <time>))
  (time-difference t0 t1))

(define (time-difference->seconds td)
  (+ (time-second td)
     (/ (time-nanosecond td) 1e9)))

(define* (simulate make-skeleton add-actors!
                   #:key (width 500) (height 500) (duration #f))
  (define (load)
    (set! sim-t0 (elapsed-time))
    (set! world (make-skeleton))
    (add-actors! world)
    (set! skeleton-canvas (make-skeleton-canvas world)))

  (define (update delta-t)
    (if (and duration (> (- (elapsed-time) sim-t0) duration))
        (quit))
    (let* ((t0      (current-time))
           (world++ (make-skeleton))
           (++      (make-++ world world++)))
      (for-each
       ;; mutate `world++' via `++', inserting a new actor
       (cut advance$ ++ <>)
       (get-actors world))
      (set! world world++)
      (insert! update-times
               (time-difference->seconds (- (current-time) t0)))))

  (define (draw alpha)
    (define t0 (current-time))
    (draw-canvas skeleton-canvas)
    (draw-canvas (make-actors-canvas world))
    (insert! draw-times
             (time-difference->seconds (- (current-time) t0))))

  (define (quit)
    (define (avg l) (/ (apply + l) (length l)))
    (pk 'elapsed-time    (- (elapsed-time) sim-t0))
    (pk 'avg-draw-time   (exact->inexact (avg draw-times)))
    (pk 'avg-update-time (exact->inexact (avg update-times)))
    (pk 'draw-count      (length draw-times))
    (pk 'update-count    (length update-times))

    (abort-game))

  (run-game
   #:load load
   #:update update
   #:update-hz (recip *simulate/time-step*)
   #:draw draw
   #:quit quit
   #:window-width width
   #:window-height height))
