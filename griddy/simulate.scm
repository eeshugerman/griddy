(define-module (griddy simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (griddy core)
  #:use-module (griddy util)
  ;; TODO: Better API. <simulation> class? closure-based iterator?
  #:export (get-first
            iterate))

(define *simulate/fps* 5)
(define *simulate/time-step* (/ 1 *simulate/fps*))

(define (make-get-static++ world world++)
  (define lookup-table
    (alist->hash-table
     (zip-to-alist (get world 'static-items)
                   (get world++ 'static-items))
     eq?))
  (lambda (world-item)
    (hash-table-ref lookup-table world-item)))

(define-method (get-first (make-skeleton <procedure>) (add-actors! <procedure>))
  (define world (make-skeleton))
  (add-actors! world)
  world)

(define-method (iterate (make-skeleton <procedure>) (world <world>))
  (let* ((world++ (make-skeleton))
         (get-static++ (make-get-static++ world world++)))
    (for-each
     ;; mutate `world++' via `get-static++', inserting
     ;; a new/++ `actor'
     (cut advance! <> get-static++)
     (get-actors world))
    world++))


(define (pop-route! actor)
  (slot-set! actor 'route (cdr (slot-ref actor 'route))))

(define gte-1 (cut (>= <> 1)))
(define lte-0 (cut (<= <> 0)))

(define-method (advance! (actor <actor>) (get-static++ <procedure>))
  (let* ((lane-current (get actor 'location 'road-lane))
         (direction-current (get lane-current 'direction))
         (pos-param-current (get actor 'location 'pos-param))
         (pos-param-max-delta (* *simulate/time-step*
                                 (/ (get actor 'max-speed)
                                    (length-of (get lane-current 'segment)))
                                 (match direction-current
                                   ('forward +1)
                                   ('backward -1))))
         (actor++ (copy actor get-static++)))

    (define (do/=nil=)
           (link! actor++ (make <location>
                            #:road-lane lane++
                            #:pos-param (get actor 'location 'pos-param))))

    (define (do/arrive-at pos-param-target)
      (let* ((done?
              (>= (abs pos-param-max-delta)
                  (abs (- pos-param-target pos-param-current))))
             (pos-param-next
              (if done?
                  pos-param-target
                  (+ pos-param-current pos-param-max-delta))))
        (if done? pop-route! actor++)
        (link! actor++ (make <location>
                         #:road-lane (get-static++ lane-current)
                         #:pos-param pos-param-next))))

    (define (do/turn-onto lane-next)
      (let* ((pos-param-dumb-next
              (+ pos-param-current pos-param-max-delta))
             (direction-next
              (get lane-next 'direction))
             (pos-param-next
              (match (list direction-current direction-next)
                (('forward 'forward)    (- pos-param-dumb-next 1))
                (('forward 'backward)   (- 1 (- pos-param-dumb-next 1)))
                (('backward 'forward)   (- pos-param-dumb-next))
                (('backward 'backward)  (- 1 (- pos-param-dumb-next)))))
             (done?
              (match (list direction-current pos-param-dumb-next)
                (('forward (? gte-1))   #t)
                (('backward (? lte-0))  #t)
                (_                      #f)))
             (location++ (make <location>
                           #:road-lane (get-static++ lane-next)
                           #:pos-param pos-param-next)))
        (link! actor++ location++)
        (if done? (pop-route! actor++))))

    (match (get actor 'route)
      (()
       (do/=nil=))
      ((('arrive-at pos-param) rest ...)
       (do/arrive-at pos-param))
      ((('turn-onto road-lane) rest ...)
       (do/turn-onto road-lane))
      )))
