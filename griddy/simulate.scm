(define-module (griddy simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:use-module (griddy draw)
  #:use-module (griddy simulate route)
  #:export (simulate))


(define (make-++ world world++)
  (define static-items-table
    (alist->hash-table
     (zip-to-alist (get world 'static-items)
                   (get world++ 'static-items))
     eq?))

  (define-generic ++)

  (define-method (++ obj)
    obj)

  (define-method (++ (item <static>))
    (hash-table-ref static-items-table item))

  (define-method (++ (route <route>))
    (define (copy-step step)
      (map ++ step))
    (make <route>
      #:steps (map copy-step (get route 'steps))))

  (define-method (++ (actor <actor>))
    (define new-actor (make <actor>))
    (define (copy-slot-if-bound! slot)
      (if (slot-bound? actor slot)
          (slot-set! new-actor
                     slot
                     (++ (slot-ref actor slot)))))
    (copy-slot-if-bound! 'max-speed)
    (copy-slot-if-bound! 'route)
    new-actor)
  ++)

(define (do-nothing actor ++)
  (lambda ()
    (link! (++ actor) (make <location>
                        #:road-lane (++ (get actor 'location 'road-lane))
                        #:pos-param (get actor 'location 'pos-param)))))

(define-method (advance! (actor <actor>) (++ <generic>))
  (match actor
    ((and (= 'agenda ())
          (= 'route (= 'steps ())))
     (do-nothing actor ++))
    ((and (= 'agenda ('travel-to location))
          (= 'route (= 'steps ())))
     (route-set! actor (find-route actor location)))
    ((and (= 'agenda ())
          (= 'route (= 'steps (item attrs ...))))
     (advance/route! actor ++))
    ((and (= 'agenda (item attrs ...))
          (= 'route (= 'steps (item attrs ...))))
     ;; what do? overwrite or append?
     (throw 'unimplemented)))

(define world #f)

(define (simulate make-skeleton add-actors!)
  (define (load)
    (set! world (make-skeleton))
    (add-actors! world))

  (define (update delta-t)
    (let* ((world++ (make-skeleton))
           (++ (make-++ world world++)))
      (for-each
       ;; mutate `world++' via `++', inserting a new `actor++'
       (cut advance! <> ++)
       (get-actors world))
      (set! world world++)))

  (run-game
   #:load load
   #:update update
   #:draw (lambda (alpha) (draw-world world))
   #:window-width 500
   #:window-height 500))
