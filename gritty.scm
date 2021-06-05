(use-modules (oop goops))

(define (slot-push! obj slot val)
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <world> ()
  (grid
   #:init-keyword #:grid
   #:getter grid)
  (road-junctions
   #:init-form '()
   #:accessor road-junctions)
  (road-segments
   #:init-form '()
   #:accessor road-segments)
  (actors
   #:init-form '()
   #:accessor actors))

(define-method (get-tile (w <world>) (x <integer>) (y <integer>))
  (array-ref (slot-ref (grid world) 'arr) x y))

(define-class <grid> ()
  (x-size
   #:init-keyword #:x
   #:getter x-size)
  (y-size
   #:init-keyword #:y
   #:getter y-size)
  arr)

(define-method (initialize (self <grid>) . args)
  (next-method)
  (let ((arr (make-array *unspecified*
                         (x-size self)
                         (y-size self))))
    (array-map! arr (lambda () (make <tile>)))
    (slot-set! self 'arr arr))
  self)

(define-class <tile> ()
  (road-junction #:accessor road-junction)
  (road-segments
   #:init-form '()
   #:getter road-segments)
  (actors
   #:init-form '()
   #:getter actors))

(define-class <road-junction> ()
  (x-pos
   #:init-keyword #:x
   #:getter x-pos)
  (y-pos
   #:init-keyword #:y
   #:getter y-pos)
  (road-segments
   #:init-form '()
   #:accessor segments))

(define-class <road-segment> ()
  (start #:getter get-start)
  (stop #:getter get-stop)
  (lanes
   #:init-keyword #:lanes
   #:init-form '(1 . 1)))

(define-method (link! (j1 <road-junction>)
                      (s <road-segment>)
                      (j2 <road-junction>))
  (unless (or (= (x-pos j1) (x-pos j2))
              (= (y-pos j1) (y-pos j2)))
    (error "diagonal road segments are not supported"))
  (slot-push! j1 'road-segments s)
  (slot-push! j2 'road-segments s)
  (slot-set! s 'start j1)
  (slot-set! s 'stop j2))

(define-method (add! (w <world>) (j <road-junction>))
  (slot-push! w 'road-junctions j)
  (set! (road-junction (get-tile w (x-pos j) (y-pos j)))
        j))

(define-method (add! (w <world>) (s <road-segment>))
  (unless (and (slot-bound? s 'start)
               (slot-bound? s 'stop))
    (error "road segment must be linked to two junctions"))
  (slot-push! w 'road-segments s)
  (let ((start-x (x-pos (get-start s)))
        (start-y (y-pos (get-start s)))
        (stop-x (x-pos (get-stop s)))
        (stop-y (y-pos (get-stop s))))
    (cond
     ((= start-x stop-x)
      (do ((y (1+ start-y) (1+ y))
           (x start-x))
          ((>= y stop-y))
        (slot-push! (get-tile w x y) 'road-segments s)))
     ((= start-y stop-y)
      (do ((x (1+ start-x) (1+ x))
           (y start-y))
          ((>= x stop-x))
        (slot-push! (get-tile w x y) 'road-segments s)))
     (else
      (error "encountered diagonal road segment")))))


;; ---------------------------------------------------------

(define world (make <world> #:grid (make <grid>
                                     #:x 10
                                     #:y 10)))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 0 #:y 9))
      (s (make <road-segment>)))
  (link! j1 s j2 )
  (add! world j1)
  (add! world j2)
  (add! world s))
