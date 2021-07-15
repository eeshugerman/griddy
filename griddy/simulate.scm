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

(define-method (advance! (actor <actor>) (get-static++ <procedure>))
  (let* ((lane-current (get actor 'location 'road-lane))
         (direction-current (get lane-current 'direction))
         (pos-param-current (get actor 'location 'pos-param))
         (pos-param-delta (* *simulate/time-step*
                             (/ (get actor 'max-speed)
                                (length-of (get lane-current 'segment)))
                             (match direction-current
                               ('forward +1)
                               ('backward -1))))
         (lane++ (get-static++ lane-current))
         (actor++ (copy actor)))


    (define (do/=nil=)
           (link! actor++ (make <location>
                            #:road-lane lane++
                            #:pos-param (get actor 'location 'pos-param))))

    (define (do/arrive-at pos-param-target)
      (let* ((pos-param-to-target (- pos-param-target pos-param-current)))
        (if (>= (abs pos-param-delta) (abs pos-param-to-target))
            (let ((location++ (make <location>
                                #:road-lane lane++
                                #:pos-param pos-param-target)))
              (pop-route! actor++)
              (link! actor++ location++))
            ;; continue on segment
            (let* ((pos-param++ (+ pos-param-current pos-param-delta))
                   (location++ (make <location>
                                 #:road-lane lane++
                                 #:pos-param pos-param++)))
              (link! actor++ location++)))))

    (define (do/turn-onto lane-next)
      (let* ((pos-param++ (+ pos-param-current pos-param-delta))
             (lane-next++ (get-static++ lane-next)))

        (match (cons current-direction pos-param++)
          (('forward .  (? > pos-param++ 1)) ;; time to turn
           ...
           (pop-route! actor++)
           (link! actor++ location++))
          (('backward . (? < pos-param++ 0)) ;; time to turn
           ...
           (pop-route! actor++)
           (link! actor++ location++))
          ((' forward . _)
           )
          (('backward . _)
           ))))

    (match (get actor 'route)
      (()
       (do/=nil=))
      ((('arrive-at pos-param) rest ...)
       (do/arrive-at pos-param))
      ((('turn-onto road-lane) rest ...)
       (do/turn-onto road-lane))
      )))
