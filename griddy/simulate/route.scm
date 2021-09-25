;; (define-module (griddy simulate route)
;;   #:use-module (srfi srfi-26)
;;   #:use-module (ice-9 match)
;;   #:use-module (oop goops)
;;   #:use-module (oop goops describe)
;;   #:use-module (chickadee math path-finding)
;;   #:use-module (griddy core)
;;   #:use-module (griddy util)
;;   #:use-module (griddy math)
;;   #:duplicates (warn merge-generics)
;;   #:export (find-route
;;             advance-on-route$
;;             next-step)
;;   #:re-export (<actor>
;;                <location>
;;                <location/on-road>
;;                <location/off-road>))


;; workaround for goops/module funkiness
(use-modules (chickadee math path-finding)
             (griddy math))

;; assumes all road have unbroken medians
(define-method (find-route (init <location/on-road>) (dest <location/on-road>))
  (define (neighbors lane)
    (filter (cut neq? lane <>)
            (get-outgoing-lanes (ref lane
                                     'segment
                                     'junction
                                     (match-direction lane 'end 'beg)))))

  (define (cost lane-1 lane-2)
    "actual cost of moving between neighboring nodes"
    ;; TODO: not sure about this
    (get-length (ref lane-1 'segment)))

  (define (distance lane-1 lane-2)
    "approximate cost of moving between nodes"
    (l2 (get-midpoint (ref lane-1 'segment))
        (get-midpoint (ref lane-2 'segment))))

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

(define (get-pos-param-delta-max actor)
  (* (match (ref actor 'location 'road-lane 'direction)
       ('forw +1)
       ('back -1))
     (ref actor 'max-speed)
     *simulate/time-step*
     (/ 1 (get-length (ref actor 'location 'road-lane)))))

(define (route-step/arrive-at$ ++ actor pos-param-target)
  (let* ((lane-current        (ref actor 'location 'road-lane))
         (direction-current   (ref lane-current 'direction))
         (pos-param-current   (ref actor 'location 'pos-param))
         (pos-param-delta-max (get-pos-param-delta-max actor))
         (done
          (>= (abs pos-param-delta-max)
              (abs (- pos-param-target pos-param-current))))
         (pos-param-next
          (if done
              pos-param-target
              (+ pos-param-current pos-param-delta-max))))
    (when done
      (route-pop! (++ actor)))
    (link! (++ actor) (make <location/on-road>
                        #:pos-param (++ pos-param-next)
                        #:road-lane (++ lane-current)))))

(define (route-step/turn-onto$ ++ actor lane-next)
  (let* ((lane-current         (ref actor 'location 'road-lane))
         (direction-current    (ref lane-current 'direction))
         (pos-param-current    (ref actor 'location 'pos-param))
         (pos-param-delta-max  (get-pos-param-delta-max actor))
         (pos-param-next-naive (+ pos-param-current pos-param-delta-max))
         (direction-next       (ref lane-next 'direction))
         (done
          (match `(,direction-current ,pos-param-next-naive)
            (('forw (? (cut >= <> 1))) #t)
            (('back (? (cut <= <> 0))) #t)
            (_ #f)))
         (pos-param-next
          (match `(,done ,direction-current ,direction-next)
            ((#f _ _) pos-param-next-naive)
            ;; TODO: using `pos-param-next-naive' here assumes
            ;;       (= (get-length lane-current) (get-length lane-next))
            ((#t 'forw 'forw) (- pos-param-next-naive 1))
            ((#t 'forw 'back) (- 1 (- pos-param-next-naive 1)))
            ((#t 'back 'forw) (- pos-param-next-naive))
            ((#t 'back 'back) (- 1 (- pos-param-next-naive))))))
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
