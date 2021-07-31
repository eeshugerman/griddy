(define-module (griddy simulate route)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (chickadee math path-finding)
  #:use-module (griddy core)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:export (find-route
            advance/route!))

(define *simulate/fps* 25)
(define *simulate/time-step* (/ 1 *simulate/fps*))

(define-method (pop-step! (route <route>))
  (slot-set! route 'steps (cdr (slot-ref route 'steps))))

(define-method (next-step (route <route>))
  (if (null? (get route 'steps))
      (list)
      (car (get route 'steps))))

(define (get-pos-param-delta-max actor)
  (* (match (get actor 'location 'road-lane 'direction)
       ('forw +1)
       ('back -1))
     (get actor 'max-speed)
     *simulate/time-step*
     (/ 1 (get-length (get actor 'location 'road-lane 'segment)))))

(define (route-step/arrive-at actor ++)
  (lambda (pos-param-target)
    (let* ((actor++ (++ actor))
           (lane-current (get actor 'location 'road-lane))
           (direction-current (get lane-current 'direction))
           (pos-param-current (get actor 'location 'pos-param))
           (pos-param-delta-max (get-pos-param-delta-max actor))
           (done
            (>= (abs pos-param-delta-max)
                (abs (- pos-param-target pos-param-current))))
           (pos-param-next
            (if done
                pos-param-target
                (+ pos-param-current pos-param-delta-max))))
      (when done
        (pop-step! (get actor++ 'route))
        (agenda-pop! actor))
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
           (done
            (match (list direction-current pos-param-next-naive)
              (('forw (? (cut >= <> 1))) #t)
              (('back (? (cut <= <> 0))) #t)
              (_ #f)))
           (pos-param-next
            (match (list done direction-current direction-next)
              ((#f _ _) pos-param-next-naive)
              ;; TODO: using `pos-param-next-naive' here assumes
              ;;       (= (get-length lane-current) (get-length lane-next))
              ((#t 'forw 'forw) (- pos-param-next-naive 1))
              ((#t 'forw 'back) (- 1 (- pos-param-next-naive 1)))
              ((#t 'back 'forw) (- pos-param-next-naive))
              ((#t 'back 'back) (- 1 (- pos-param-next-naive))))))
      (link! actor++ (make <location>
                       #:road-lane (++ (if done lane-next lane-current))
                       #:pos-param pos-param-next))
      (if done (pop-step! (get actor++ 'route))))))

(define (neighbors lane)
  (get-outgoing-lanes
   (get lane
        'segment
        (match-direction lane
          'stop-junciton
          'start-junction))))

(define (cost lane-1 lane-2)
  "actual cost of moving between neighboring nodes"
  (get-length (get lane-1 'segment))) ;; TODO: not sure about this

(define (distance lane-1 lane-2)
  "approximate cost of moving between nodes"
  (l2 (get-midpoint (get lane-1 'segment))
      (get-midpoint (get lane-2 'segment))))

;; TODO: why doesn't this work as a method?
;; (define-method (find-route (actor <actor>) (loc <location>))
(define (find-route actor loc)
  (let* ((route-finder
          (make-path-finder))  ;; TODO: safe to reuse this?
         (start-lane
          (get actor 'location 'road-lane))
         (stop-lane
          (get loc 'road-lane))
         (lanes
          (a* route-finder start-lane stop-lane neighbors cost distance))
         (lane->route-step
          (cut cons 'turn-onto <>))
         (pos-param->route-step
          (cut cons 'arrive-at <>))
         (steps
          (append (map lane->route-step lanes)
                  (list (pos-param->route-step (get loc 'pos-param))))))

    (make <route> #:steps steps)))

;; TODO: why doesn't this work with <actor>?
(define-method (advance/route! (actor <object>) (++ <generic>))
  ;; TODO: don't use 'arrive-at/'turn-onto, just <lane> or <number>
  (match (next-step (get actor 'route))
    (('arrive-at pos-param)
     ((route-step/arrive-at actor ++) pos-param))
    (('turn-onto road-lane)
     ((route-step/turn-onto actor ++) road-lane))))
