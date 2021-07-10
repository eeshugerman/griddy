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
  (let* ((actor-next (copy actor))
         (lane-current (get actor 'location 'road-lane))
         (lane-next (get-next-static lane-current))
         (pos-param-next (+ 0.1 (get actor 'location 'pos-param)))
         (location-next (make <location>
                          #:road-lane lane-next
                          #:pos-param pos-param-next)))
    (link! actor-next location-next)))
