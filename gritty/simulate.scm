(define-module (gritty simulate)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (gritty core)
  #:export (get-first
            get-next))

;; TODO: Better API. <simulation> class? closure-based iterator?

(define (get-next-world-location current-world-location next-world)
  ;; how??? is it possible?
  ;; also, move into `get-next` so `next-world' param is not needed
  )

(define (get-first (make-skeleton add-actors!))
  (let ((world (make-skeleton)))
    (add-actors! world)
    world))

(define (get-next (make-skeleton current-world))
  (let ((next-world (make-skeleton)))
    (for-each
     (lambda (actor)
       (let* ((current-world-location
               (-> actor 'location))
              (next-world-location
               (get-next-world-location current-world-location next-world)))
         ;; mutate `next-world' via `next-world-location'
         (advance! actor next-world-location)))
     (get-actors current-world))))
