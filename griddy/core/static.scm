(define-module (griddy core static)
  #:duplicates (merge-generics)
  #:use-module (srfi srfi-69)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (chickadee math bezier)
  #:use-module (chickadee math vector)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:use-module (griddy math)
  #:use-module (griddy core position)
  #:export (<road-junction>
            <road-lane>
            <road-segment>
            <road-lane/segment>
            <road-lane/junction>
            <static>
            get-lanes
            get-lane-count))

(util:extend-primitives!)
(math:extend-primitives!)

(define make-hash-table (@ (srfi srfi-69) make-hash-table))

(define-class <static> ())

(define-class <road-lane> (<static>)
  (actors
   #:init-thunk list))

(define-class <road-lane/segment> (<road-lane>)
  segment
  (direction  ;; 'forw or 'back, relative to segment
   #:init-keyword #:direction)
  rank) ;; 0..

(define-class <road-lane/junction> (<road-lane>)
  (junction #:init-keyword #:junction)
  curve)

(define-method (initialize (self <road-lane/junction>) initargs)
  (define (get-junction lane lane-type)
    (ref lane
         'segment
         'junction
         (match `(,lane-type ,(ref lane 'direction))
           (('in  'forw) 'end)
           (('in  'back) 'beg)
           (('out 'forw) 'beg)
           (('out 'back) 'end))))

  (let ((in-lane  (get-keyword #:in-lane initargs))
        (out-lane (get-keyword #:out-lane initargs)))
    (unless (eq? (get-junction in-lane 'in)
                 (get-junction out-lane 'out))
      (throw 'no-connection in-lane out-lane))
    (let* ((junction (get-junction in-lane 'in))
           (offset   (* 2
                        (get-radius junction)
                        (/ *road-segment/wiggle-room-%* 100)))
           (p0       (get-pos in-lane  'end))
           (p3       (get-pos out-lane 'beg))
           (p1       (+ p0 (* offset (get-tangent-vec in-lane))))
           (p2       (- p3 (* offset (get-tangent-vec out-lane))))
           (curve    (make-bezier-curve p0 p1 p2 p3)))
      (set! (ref self 'junction) junction)
      (set! (ref self 'curve) curve)))
  (next-method))

(define-class <road-junction> (<static>)
  pos ;; vec2
  (segments
   #:init-thunk list)
  (lane-map
   #:init-form `((inputs  . ,(make-hash-table eq?))
                 (outputs . ,(make-hash-table eq?)))))

(define-method (initialize (self <road-junction>) initargs)
  (set! (ref self 'pos) (vec2 (get-keyword #:x initargs)
                              (get-keyword #:y initargs)))
  (next-method))

(define-class <road-segment> (<static>)
  (actors ;; off-road only, otherwise they belong to lanes
   #:init-thunk list)
  (junction
   #:init-form `((beg . ())
                 (end . ())))
  ;; lane lists are ascending by rank
  ;; higher rank means further from center/median
  (lanes
   #:init-form '((forw . ())
                 (back . ())))
  (lane-count
   #:init-form '((forw . 0)
                 (back . 0))))

(define-method (initialize (self <road-segment>) initargs)
  (next-method)
  (define alist-slots '(junction lanes lane-count))
  (define (make-mutable! slot)
    (set! (ref self slot) (copy-tree (ref self slot))))
  (for-each make-mutable! alist-slots))

(define-method (get-lanes (junction <road-junction>))
  "get all junction lanes for a junction"
  (hash-table-keys (ref junction 'lane-map 'outputs)))

(define-method (get-lanes (segment <road-segment>))
  (append (ref segment 'lanes 'forw)
          (ref segment 'lanes 'back)))

(define-method (get-lanes (segment <road-segment>) (direction <symbol>))
  (ref segment 'lanes direction))

(define-method (get-lane-count (segment <road-segment>))
  (+ (ref segment 'lane-count 'forw)
     (ref segment 'lane-count 'back)))

(define-method (get-lane-count (segment <road-segment>) (direction <symbol>))
  (ref segment 'lane-count direction))
