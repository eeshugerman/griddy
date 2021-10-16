;; (define-module (griddy simulate route)
;;   #:use-module (srfi srfi-26)
;;   #:use-module (ice-9 match)
;;   #:use-module (oop goops)
;;   #:use-module (oop goops describe)
;;   #:use-module (chickadee data path-finding)
;;   #:use-module (griddy constants)
;;   #:use-module (griddy util)
;;   #:use-module (griddy math)
;;   #:use-module (griddy core)
;;   #:export (find-route
;;             advance-on-route$
;;             next-step))


;; workaround for goops/module funkiness
(use-modules (chickadee data path-finding)
             (griddy math))

;; assumes all road have unbroken medians
(define-method (find-route (init <location/on-road>) (dest <location/on-road>))

  (define-generic neighbors)
  (define-method (neighbors (lane <road-lane/segment>))
    (get-junction-lanes lane))
  (define-method (neighbors (lane <road-lane/junction>))
    (list (get-segment-lane lane)))

  (define-generic cost)
  (define-method (cost (lane-1 <road-lane/segment>) (lane-2 <road-lane>))
    "actual cost of moving between neighboring nodes"
    ;; TODO: not sure about this
    (get-length (ref lane-1 'segment)))

  (define-method (cost (lane-1 <road-lane/junction>) (lane-2 <road-lane>))
    "actual cost of moving between neighboring nodes"
    ;; TODO: not sure about this
    0)

  (define (distance lane-1 lane-2)
    "approximate cost of moving between nodes"
    (l2 (get-midpoint lane-1)
        (get-midpoint lane-2)))

  (let* ((lanes (a* (make-path-finder)
                    (ref init 'road-lane)
                    (ref dest 'road-lane)
                    neighbors
                    cost
                    distance))
         (lane->route-step (cut list 'turn-onto <>))
         (pos-param->route-step (cut list 'arrive-at <>)))
    (extend (map lane->route-step (cdr lanes))
              (pos-param->route-step (ref dest 'pos-param)))))

(define* (get-pos-param-delta
          actor
          #:key
          (time-step *simulate/time-step*)
          (speed     (ref actor 'max-speed))
          (road-lane (ref actor 'location 'road-lane)))
  (* speed time-step (recip (get-length road-lane))))

(define (route-step/arrive-at$ ++ actor pos-param-target)
  (let* ((lane-current        (ref actor 'location 'road-lane))
         (direction-current   (ref lane-current 'direction))
         (pos-param-current   (ref actor 'location 'pos-param))
         (pos-param-delta-max (get-pos-param-delta actor))
         (done                (>= pos-param-delta-max
                                  (- pos-param-target
                                     pos-param-current)))
         (pos-param-next      (if done
                                  pos-param-target
                                  (+ pos-param-current
                                     pos-param-delta-max))))
    (when done
      (route-pop! (++ actor)))
    (link! (++ actor) (make <location/on-road>
                        #:pos-param (++ pos-param-next)
                        #:road-lane (++ lane-current)))))

(define (route-step/turn-onto$ ++ actor lane-next)
  (let* ((lane-current          (ref actor 'location 'road-lane))
         (pos-param-current     (ref actor 'location 'pos-param))
         (pos-param-delta       (get-pos-param-delta actor))
         (pos-param-next-naive  (+ pos-param-current pos-param-delta))
         (done                  (>= pos-param-next-naive 1))
         (pos-param-next
          (if done
              (let* ((fraction-time-spent
                      (/ (- 1 pos-param-current) pos-param-delta))
                     (time-remaining
                      (* (- 1 fraction-time-spent)
                         *simulate/time-step*)))
                (get-pos-param-delta actor
                                     #:time-step time-remaining
                                     #:road-lane lane-next))
              pos-param-next-naive)))
    (when done
      (route-pop! (++ actor)))

    (link! (++ actor) (make <location/on-road>
                        #:road-lane (++ (if done lane-next lane-current))
                        #:pos-param (++ pos-param-next)))))

(define-method (advance-on-route$ (++ <generic>) (actor <actor>))
  ;; TODO: maybe don't use 'arrive-at/'turn-onto, just <lane> or <number>
  (match (car (ref actor 'route))
    (('arrive-at pos-param) (route-step/arrive-at$ ++ actor pos-param))
    (('turn-onto road-lane) (route-step/turn-onto$ ++ actor road-lane))))
