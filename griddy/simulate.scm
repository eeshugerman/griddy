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
  #:duplicates (merge-generics)
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

  (define-method (++ (list' <list>))
    (map ++ list'))

  (define-method (++ (item <static>))
    (hash-table-ref static-items-table item))

  (define-method (++ (location <location>))
    (make <location>
      #:road-lane (++ (get location 'road-lane))
      #:pos-param (get location 'pos-param)))

  (define-method (++ (route <route>))
    (make <route>
      #:steps (++ (get route 'steps))))

  (define-method (++ (actor <actor>))
    (define new-actor (make <actor>))
    (define (copy-slot-if-bound! slot)
      (if (slot-bound? actor slot)
          (slot-set! new-actor slot (++ (slot-ref actor slot)))))
    (for-each copy-slot-if-bound!
              '(max-speed agenda route))
    new-actor)

  ++)

(define (do-nothing actor ++)
  (link! (++ actor) (++ (get actor 'location))))

(define-method (advance! (actor <actor>) (++ <generic>))
  (match (list (get actor 'agenda) (get actor 'route 'steps))
    ((() ())
     (do-nothing actor ++))
    (((('travel-to dest) _ ...) ())
     (let ((actor++ (++ actor)))
       (set-route! actor++ (++ (find-route actor dest)))
       (link! actor++ (++ (get actor 'location)))))
    (((('travel-to dest) _ ...) (_ ..1))
     (advance/route! actor ++))))

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
