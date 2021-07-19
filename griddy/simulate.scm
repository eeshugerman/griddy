(define-module (griddy simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:use-module (griddy draw-chickadee)
  #:export (simulate))

(define *simulate/fps* 25)
(define *simulate/time-step* (/ 1 *simulate/fps*))

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

(define (get-pos-param-delta-max actor)
  (* (match (get actor 'location 'road-lane 'direction)
       ('forw +1)
       ('back -1))
     (get actor 'max-speed)
     *simulate/time-step*
     (/ 1 (length-of (get actor 'location 'road-lane 'segment)))))

(define (route-step/=nil= actor ++)
  (lambda ()
    (link! (++ actor) (make <location>
                        #:road-lane (++ (get actor 'location 'road-lane))
                        #:pos-param (get actor 'location 'pos-param)))))

(define (route-step/arrive-at actor ++)
  (lambda (pos-param-target)
    (let* ((actor++ (++ actor))
           (lane-current (get actor 'location 'road-lane))
           (direction-current (get lane-current 'direction))
           (pos-param-current (get actor 'location 'pos-param))
           (pos-param-delta-max (get-pos-param-delta-max actor))
           (done?
            (>= (abs pos-param-delta-max)
                (abs (- pos-param-target pos-param-current))))
           (pos-param-next
            (if done?
                pos-param-target
                (+ pos-param-current pos-param-delta-max))))
      (if done? (pop-step! (get actor++ 'route)))
      (link! actor++ (make <location>
                       #:road-lane (++ lane-current)
                       #:pos-param pos-param-next)))))

(define (route-step/turn-onto actor ++)
  (lambda (lane-next)
    (let* ((actor++ (++ actor))
           (lane-current (get actor 'location 'road-lane))
           (direction-current (get lane-current 'direction))
           (pos-param-current (get actor 'location 'pos-param))
           (pos-param-delta-max (get-pos-param-delta-max actor))
           (pos-param-next-naive
            (+ pos-param-current pos-param-delta-max))
           (direction-next
            (get lane-next 'direction))
           (done?
            (match (list direction-current pos-param-next-naive)
              (('forw (? (cut >= <> 1))) #t)
              (('back (? (cut <= <> 0))) #t)
              (_ #f)))
           (pos-param-next
            (match (list done? direction-current direction-next)
              ((#f _ _) pos-param-next-naive)
              ;; TODO: using `pos-param-next-naive' here assumes
              ;;       (= (length-of lane-current) (length-of lane-next))
              ((#t 'forw 'forw) (- pos-param-next-naive 1))
              ((#t 'forw 'back) (- 1 (- pos-param-next-naive 1)))
              ((#t 'back 'forw) (- pos-param-next-naive))
              ((#t 'back 'back) (- 1 (- pos-param-next-naive))))))
      (link! actor++ (make <location>
                       #:road-lane (++ (if done? lane-next lane-current))
                       #:pos-param pos-param-next))
      (if done? (pop-step! (get actor++ 'route))))))

(define-method (advance! (actor <actor>) (++ <generic>))
  (match (next-step (get actor 'route))
    (()
     ((route-step/=nil= actor ++)))
    (('arrive-at pos-param)
     ((route-step/arrive-at actor ++) pos-param))
    (('turn-onto road-lane)
     ((route-step/turn-onto actor ++) road-lane))))

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
