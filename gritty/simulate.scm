(define-module (gritty simulate)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (gritty core)
  #:use-module (gritty util)
  ;; TODO: Better API. <simulation> class? closure-based iterator?
  #:export (get-first
            get-next))

(define *simulate/fps* 5)
(define *simulate/time-step* (/ 1 *simulate/fps*))

(define (make-next-static-getter current-world next-world)
  (define lookup-table
    (alist->hash-table
     (zip-to-alist (get current-world 'static-items)
                   (get next-world 'static-items))
     eq?))
  (lambda (current-world-item)
    (hash-table-ref lookup-table current-world-item)))

(define-method (get-first (make-skeleton <procedure>) (add-actors! <procedure>))
  (define world (make-skeleton))
  (add-actors! world)
  world)

(define-method (get-next (make-skeleton <procedure>) (current-world <world>))
  (let* ((next-world (make-skeleton))
         (get-next-static (make-next-static-getter current-world next-world)))
    (for-each
     ;; mutate `next-world' via `get-next-static', inserting
     ;; a new/next `actor'
     (cut advance! <> get-next-static)
     (get-actors current-world))
    next-world))

(define-method (advance! (actor <actor>) (get-next-static <procedure>))

  (define (do/=nil=)
    (let* ((lane-current (get actor 'location 'road-lane))
           (lane-next (get-next-static lane-current))
           (actor-next (copy actor))
           (location-next (make <location>
                        #:road-lane lane-next
                        #:pos-param (get actor 'location 'pos-param))))
      (link! actor-next location-next)))

  (define (do/arrive-at pos-param-target)
    (let* ((lane-current (get actor 'location 'road-lane))
           (lane-next (get-next-static lane-current))
           (pos-param-current (get actor 'location 'pos-param))
           (pos-param-delta (* *simulate/time-step*
                               (/ (get actor 'max-speed)
                                  (length-of (get lane-current 'segment)))))
           (pos-param-to-go (- pos-param-target pos-param-current)))
      (if (>= (abs pos-param-delta) (abs pos-param-to-go))
          ;; arrived
          (let ((actor-next (copy actor))
                (location-next (make <location>
                                 #:road-lane lane-next
                                 #:pos-param pos-param-target)))
            (slot-set! actor-next 'route '())
            (link! actor-next location-next))
          ;; still travelling
          (let* ((actor-next (copy actor))
                 (pos-param-next ((if (> pos-param-target pos-param-current) + -)
                                  pos-param-current
                                  pos-param-delta))
                 (location-next (make <location>
                                  #:road-lane lane-next
                                  #:pos-param pos-param-next)))
            (link! actor-next location-next)))))

  (match (get actor 'route)
    (()
     (do/=nil=))
    (((arrive-at target-pos-param) rest ...)
     (do/arrive-at target-pos-param))))
