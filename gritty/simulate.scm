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
     (zip-to-alist (-> current-world 'static-items)
                   (-> next-world 'static-items))
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
  (let* ((lane-current (-> actor 'location 'road-lane))
         (lane-next (get-next-static lane-current))
         (actor-next (copy actor))
         (pos-param-next (+ 0.1 (-> actor 'location 'pos-param))))
    (link! actor-next lane-next pos-param-next)))
