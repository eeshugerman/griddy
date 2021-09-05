(define-module (griddy simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69) ;; #:replace (make-hash-table)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (chickadee)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:use-module (griddy draw)
  ;; #:use-module (griddy simulate route)
  #:export (simulate))

;; workaround for goops/module funkiness
(include "simulate/route.scm")


(define (make-++ world world++)
  (define static-items-table
    (alist->hash-table
     (zip-to-alist (get world 'static-items)
                   (get world++ 'static-items))
     eq?))

  (define actors-table ((@ (srfi srfi-69) make-hash-table) eq?))

  (define-generic ++)

  (define-method (++ obj)
    obj)

  (define-method (++ (list' <list>))
    (map ++ list'))

  (define-method (++ (item <static>))
    (hash-table-ref static-items-table item))

  (define-method (++ (location <location-on-road>))
    (make <location-on-road>
      #:pos-param (get location 'pos-param)
      #:road-lane (++ (get location 'road-lane))))

  (define-method (++ (location <location-off-road>))
    (make <location-off-road>
      #:pos-param (get location 'pos-param)
      #:road-segment (get location 'road-segment)
      #:road-side-direction (get location 'road-side-direction)))

  (define-method (++ (actor <actor>))
    (hash-table-ref
     actors-table
     actor
     (lambda ()
       (define new-actor (make <actor>))
       (define (copy-slot-if-bound! slot)
         (if (slot-bound? actor slot)
             (slot-set! new-actor slot (++ (slot-ref actor slot)))))
       (for-each copy-slot-if-bound!
                 '(max-speed agenda route location))
       (hash-table-set! actors-table actor new-actor)
       new-actor)))

  ++)

(define-method (do-nothing$ (++ <generic>) (actor <actor>))
  (link! (++ actor)
         (++ (get actor 'location))))

(define-method (begin-route$ (++ <generic>) (actor <actor>))
  (let* ((actor++ (++ actor))
         (init-loc++ (off-road->on-road (get actor++ 'location)))
         (dest-loc++ (off-road->on-road (cadar (get actor++ 'agenda))))
         (route++    (find-route init-loc++ dest-loc++)))
    (set-route! actor++ route++)
    (link! actor++ init-loc++)))

(define-method (end-route$ (++ <generic>) (actor <actor>))
  (let* ((actor++    (++ actor))
         (location++ (on-road->off-road (get actor++ 'location))))
    (set-route! actor++ 'none)
    (agenda-pop! actor++)
    (link! actor++ location++)))

(define-method (advance$ (++ <generic>) (actor <actor>))
  (let ((agenda-status
         (match (get actor 'agenda)
           (()                     'nothing)
           ((('travel-to _) _ ...) 'traveling)))

        (route-status
         (match (get actor 'route)
           ('none   'none)
           ((_ ..1) 'some)
           (()      'done))))

    (match `(,agenda-status ,route-status)
      (('nothing   'none) (do-nothing$       ++ actor))
      (('traveling 'none) (begin-route$      ++ actor))
      (('traveling 'some) (advance-on-route$ ++ actor))
      (('traveling 'done) (end-route$        ++ actor))
      )))

(define world #f)

(define* (simulate make-skeleton add-actors! #:key (width 500) (height 500))
  (define (load)
    (set! world (make-skeleton))
    (add-actors! world))


  (define (update delta-t)
    (let* ((world++ (make-skeleton))
           (++ (make-++ world world++)))
      (for-each
       ;; mutate `world++' via `++', inserting a new actor
       (cut advance$ ++ <>)
       (get-actors world))
      (set! world world++)))

  (run-game
   #:load load
   #:update update
   #:draw (lambda (alpha) (draw-world world))
   #:window-width width
   #:window-height height))
