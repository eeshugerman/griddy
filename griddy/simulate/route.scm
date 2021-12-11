(define-module (griddy simulate route)
  #:duplicates (merge-generics)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (pipe)
  #:use-module (chickadee data path-finding)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core)
  #:export (find-route
            advance-on-route$
            next-step))

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

  (define-generic distance)
  (define-method (distance (lane-1 <road-lane>) (lane-2 <road-lane>))
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

(define* (get-pos-param-next
          actor
          #:key
          (current   (ref actor 'location 'pos-param))
          (time-step *simulate/time-step*)
          (speed     (ref actor 'max-speed))
          (road-lane (ref actor 'location 'road-lane)))

  ;; todo: distinguish draw size and actual size
  (let* ((tailgate-buffer    (/ (* 2 *actor/size*)
                                (get-length road-lane)))
         (delta-unobstructed (* speed
                                time-step
                                (recip (get-length road-lane))))
         (next     (+ current delta-unobstructed))
         (obstacle (bbtree-find-min (ref road-lane 'actors)
                                    current
                                    (+ next tailgate-buffer))))
    (if (null? obstacle)
        next
        (- (ref obstacle 'location 'pos-param)
           tailgate-buffer))))

(define (route-step/arrive-at$ ++ actor pos-param-target)
  (let* ((lane-current         (ref actor 'location 'road-lane))
         (direction-current    (ref lane-current 'direction))
         (pos-param-current    (ref actor 'location 'pos-param))
         (pos-param-next-naive (get-pos-param-next actor))
         (done                 (>= pos-param-next-naive
                                   pos-param-target))
         (pos-param-next       (if done
                                   pos-param-target
                                   pos-param-next-naive)))
    (when done
      (route-pop! (++ actor)))
    (link! (++ actor) (make <location/on-road>
                        #:pos-param (++ pos-param-next)
                        #:road-lane (++ lane-current)))))

(define (route-step/turn-onto$ ++ actor lane-target)
  (let* ((lane-current          (ref actor 'location 'road-lane))
         (pos-param-current     (ref actor 'location 'pos-param))
         (pos-param-next-naive  (get-pos-param-next actor))
         (done                  (>= pos-param-next-naive 1))

         (junction-full?        (lambda (junction)
                                  (->> junction
                                       (get-lanes)
                                       (map get-actors)
                                       (map length)
                                       (apply +)
                                       (< 0))))
         (waiting
          (and done
               (is-a? lane-target <road-lane/junction>)
               (junction-full? (ref lane-target 'junction))))

         (pos-param-next
          (cond
           (waiting pos-param-current)
           (done
            (let* ((time-spent      (/ (- 1 pos-param-current)
                                       (ref actor 'max-speed)))
                   (time-remaining  (- *simulate/time-step*
                                       time-spent)))
              (get-pos-param-next actor
                                  #:current 0
                                  #:time-step time-remaining
                                  #:road-lane lane-target)))

           (else pos-param-next-naive)))

         (lane-next
          (cond
           (waiting lane-current)
           (done lane-target)
           (else lane-current))))

    (when (and done (not waiting))
      (route-pop! (++ actor)))
    (link! (++ actor) (make <location/on-road>
                        #:road-lane (++ lane-next)
                        #:pos-param (++ pos-param-next)))))

(define-method (advance-on-route$ (++ <generic>) (actor <actor>))
  ;; TODO: maybe don't use 'arrive-at/'turn-onto, just <lane> or <number>
  (match (car (ref actor 'route))
    (('arrive-at pos-param) (route-step/arrive-at$ ++ actor pos-param))
    (('turn-onto road-lane) (route-step/turn-onto$ ++ actor road-lane))))
