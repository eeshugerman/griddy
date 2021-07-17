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


(define-method (advance! (actor <actor>) (get-static++ <procedure>))
  (let* ((lane-current (get actor 'location 'road-lane))
         (direction-current (get lane-current 'direction))
         (pos-param-current (get actor 'location 'pos-param))
         (pos-param-max-delta (* *simulate/time-step*
                                 (/ (get actor 'max-speed)
                                    (length-of (get lane-current 'segment)))
                                 (match direction-current
                                   ('forw +1)
                                   ('back -1))))
         (actor++ (copy actor get-static++)))

    (define (do/=nil=)
           (link! actor++ (make <location>
                            #:road-lane (get-static++ lane-current)
                            #:pos-param (get actor 'location 'pos-param))))

    (define (do/arrive-at pos-param-target)
      (let* ((done?
              (>= (abs pos-param-max-delta)
                  (abs (- pos-param-target pos-param-current))))
             (pos-param-next
              (if done?
                  pos-param-target
                  (+ pos-param-current pos-param-max-delta))))
        (if done? (pop-step! (get actor++ 'route)))
        (link! actor++ (make <location>
                         #:road-lane (get-static++ lane-current)
                         #:pos-param pos-param-next))))

    (define (do/turn-onto lane-next)
      (let* ((pos-param-next-naive
              ;; TODO: this assumes (= (length-of lane-current)
              ;;                       (length-of lane-next))
              (+ pos-param-current pos-param-max-delta))
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
                ((#t 'forw 'forw) (- pos-param-next-naive 1))
                ((#t 'forw 'back) (- 1 (- pos-param-next-naive 1)))
                ((#t 'back 'forw) (- pos-param-next-naive))
                ((#t 'back 'back) (- 1 (- pos-param-next-naive)))))
             (lane++
              (get-static++ (if done? lane-next lane-current))))
        (link! actor++ (make <location>
                         #:road-lane lane++
                         #:pos-param pos-param-next))
        (if done? (pop-step! (get actor++ 'route)))))

    (match (next-step (get actor 'route))
      (()
       (do/=nil=))
      (('arrive-at pos-param)
       (do/arrive-at pos-param))
      (('turn-onto road-lane)
       (do/turn-onto road-lane))
      )))
