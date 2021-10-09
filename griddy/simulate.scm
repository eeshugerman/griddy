(define-module (griddy simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69) ;; #:replace (make-hash-table)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  ;; #:use-module (chickadee)
  #:use-module (chickadee graphics path)
  #:use-module (griddy event-loop)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:use-module (griddy draw)
  #:use-module (griddy math)
  #:use-module (griddy constants)
  ;; #:use-module (griddy simulate route)
  #:export (simulate))

;; workaround for goops/module funkiness
(include "simulate/route.scm")


(define (make-++ world world++)
  (define static-items-table
    (let loop ((table   (make-hash-table eq?))
               (items   (ref world 'static-items))
               (items++ (ref world++ 'static-items)))
      (match `(,items ,items++)
        ((()())
         table)
        (((item rest ...) (item++ rest++ ...))
         (hash-table-set! table item item++)
         (loop table rest rest++)))))

  (define actors-table ((@ (srfi srfi-69) make-hash-table) eq?))

  (define-generic ++)

  (define-method (++ obj)
    obj)

  (define-method (++ (list' <list>))
    (map ++ list'))

  (define-method (++ (item <static>))
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

(define t0 0)
(define world #f)
(define skeleton-canvas #f)

(define* (simulate make-skeleton add-actors!
                   #:key (width 500) (height 500) (length #f))
  (define (load)
    (set! t0 (elapsed-time))
    (set! world (make-skeleton))
    (add-actors! world)
    (set! skeleton-canvas (make-skeleton-canvas world)))


  (define (update delta-t)
    (if (and length (> (- (elapsed-time) t0) length))
        (abort-game))
    (let* ((world++ (make-skeleton))
           (++ (make-++ world world++)))
      (for-each
       ;; mutate `world++' via `++', inserting a new actor
       (cut advance$ ++ <>)
       (get-actors world))
      (set! world world++)))

  (define (draw alpha)
    (draw-canvas skeleton-canvas)
    (draw-canvas (make-actors-canvas world)))

  (run-game
   #:load load
   #:update update
   #:update-hz (recip *simulate/time-step*)
   #:draw draw
   #:window-width width
   #:window-height height))
