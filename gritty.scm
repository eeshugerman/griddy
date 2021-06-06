(use-modules (oop goops))

(define (slot-push! obj slot val)
  ;; todo: try `set-cdr!'?
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define-class <world> ()
  (x-size
   #:init-keyword #:x-size
   #:getter get-x-size)
  (y-size
   #:init-keyword #:y-size
   #:getter get-y-size)
  (grid
   #:getter get-grid)
  (road-junctions
   #:init-form '()
   #:getter get-road-junctions)
  (road-segments
   #:init-form '()
   #:getter get-road-segments)
  (actors
   #:init-form '()
   #:getter get-actors))

(define-method (initialize (self <world>) . args)
  (next-method)
  (let ((grid (make-array *unspecified* (get-x-size self) (get-y-size self))))
    (array-map! grid (lambda () (make <tile>)))
    (slot-set! self 'grid grid))
  self)

(define-method (get-tile (w <world>) (x <integer>) (y <integer>))
  (array-ref (get-grid w) x y))

(define-class <tile> ()
  (road-junction
   #:getter get-road-junction)
  (road-segments
   #:init-form '()
   #:getter get-road-segments)
  (actors
   #:init-form '()
   #:getter get-actors))

(define-class <road-junction> ()
  (x-pos
   #:init-keyword #:x
   #:getter get-x-pos)
  (y-pos
   #:init-keyword #:y
   #:getter get-y-pos)
  (segments
   #:init-form '()
   #:getter get-segments))

(define-class <road-segment> ()
  (start-junction #:getter get-start-junction)
  (stop-junction #:getter get-stop-junction)
  (lanes
   #:init-keyword #:lanes
   #:init-form '(1 . 1)))

(define-method (link! (j1 <road-junction>)
                      (s <road-segment>)
                      (j2 <road-junction>))
  (unless (or (= (get-x-pos j1) (get-x-pos j2))
              (= (get-y-pos j1) (get-y-pos j2)))
    (error "diagonal road segments are not supported"))
  (slot-push! j1 'segments s)
  (slot-push! j2 'segments s)
  (slot-set! s 'start-junction j1)
  (slot-set! s 'stop-junction j2))

(define-method (add! (w <world>) (j <road-junction>))
  (slot-push! w 'road-junctions j)
  (slot-set! (get-tile w (get-x-pos j) (get-y-pos j))
             'road-junction
             j))

(define-method (add! (w <world>) (s <road-segment>))
  (unless (and (slot-bound? s 'start-junction)
               (slot-bound? s 'stop-junction))
    (error "road segment must be linked to two junctions"))
  (slot-push! w 'road-segments s)
  (let ((start-x (get-x-pos (get-start-junction s)))
        (start-y (get-y-pos (get-start-junction s)))
        (stop-x (get-x-pos (get-stop-junction s)))
        (stop-y (get-y-pos (get-stop-junction s))))
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


; ---------------------------------------------------------

(define world (make <world> #:x-size 10 #:y-size 10))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 0 #:y 9))
      (s (make <road-segment>)))
  (link! j1 s j2 )
  (add! world j1)
  (add! world j2)
  (add! world s))
