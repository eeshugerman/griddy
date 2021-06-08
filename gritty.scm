(use-modules (oop goops)
             (chickadee)
             (chickadee math vector))

(define *time-step* 1/60)

(define (l2 v1 v2)
  (vec2-magnitude (vec2- v1 v2)))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <road-junction> ()
  (position ;; vec2
   #:init-keyword position
   #:accessor position)
  (segments
   #:init-form '()
   #:getter segments))

(define-class <road-segment> ()
  (start-junction #:getter start-junction)
  (stop-junction #:getter stop-junction)
  (lanes
   #:init-keyword #:lanes
   #:init-form '(1 . 1))
  (length
   #:getter length
   #:allocation #:virtual
   #:slot-ref (lambda (self)
                (l2 (position (start-junction self))
                    (position (stop-junction self))))
   #:slot-set! (lambda (_ _)
                 error "`(length <road-segment>)' is read-only")))

(define-method (link! (j1 <road-junction>)
                      (s <road-segment>)
                      (j2 <road-junction>))
  (slot-push! j1 'segments s)
  (slot-push! j2 'segments s)
  (slot-set! s 'start-junction j1)
  (slot-set! s 'stop-junction j2))

(define-class <location> ()
  (road-segment
   #:init-keyword road-segment
   #:accessor road-segment)
  (road-lane
   #:init-keyword road-lane
   #:accessor road-lane)
  (s ;; distance travelled along segment
   #:init-form 0.0
   #:init-keyword progress
   #:accessor progress)
  (direction
   #:init-form 'forward
   #:init-keyword direction
   #:accessor direction)
  ;; (position
  ;;  #:allocation #:virtual)
  )

(define-class <actor> ()
  (location
   #:init-keyword location
   #:getter location)
  (max-speed
   #:init-keyword max-speed
   #:getter max-speed)
  (agenda
   #:init-thunk (lambda () (make-agenda))
   #:accessor agenda))

(define-class <world> ()
  (road-junctions
   #:init-form '()
   #:getter road-junctions)
  (road-segments
   #:init-form '()
   #:getter road-segments)
  (actors
   #:init-form '()
   #:getter actors))

(define-method (add! (w <world>) (j <road-junction>))
  (slot-push! w 'road-junctions j))

(define-method (add! (w <world>) (s <road-segment>))
  (unless (and (slot-bound? s 'start-junction)
               (slot-bound? s 'stop-junction))
    (error "road segment must be linked to two junctions"))
  (slot-push! w 'road-segments s))

(define-method (advance! (s <road-segment>) (a <actor>))
  (let* ((speed (max-speed a))
         (distance (* speed *time-step*))
         (direction (direction (location a)))
         (op (case direction
               (('forward) +)
               (('backward) -)))
         (new-loc (op (s (location a)))))
    (match (cons direction new-loc))
      (('backward . (? negative?))
       (do-next-thing))
      (('backward . _)
       (set! (s (location a)) new-loc))
      (('forward . (? (lambda (x) (> x (length s)))))
       (set! (s (location a)) new-loc))
      (('forward . _)
       (do-next-thing))))

; ---------------------------------------------------------

(define world (make <world>))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 0 #:y 9))
      (s (make <road-segment>)))
  (link! j1 s j2 )
  (add! world j1)
  (add! world j2)
  (add! world s))

