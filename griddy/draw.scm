(define-module (griddy draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (griddy util)
  #:use-module (griddy core)
  #:use-module (griddy math)
  #:export (draw))

(define *draw/road-junction/size* 50)
(define *draw/road-junction/color* "grey")
(define *draw/road-segment/width* 50)
(define *draw/road-segment/color* "grey")
(define *draw/actor/size* 10)
(define *draw/actor/color* "yellow")


(define (square size)
  (rectangle size size))

(define-method (draw (junction <road-junction>))
  (fill (square *draw/road-junction/size*)
        *draw/road-junction/color*))

(define-method (draw (segment <road-segment>))
  (line (get segment 'start-junction 'pos-x)
        (get segment 'start-junction 'pos-y)
        (get segment 'stop-junction 'pos-x)
        (get segment 'stop-junction 'pos-y)
        #:stroke-width *draw/road-segment/width*
        #:color *draw/road-segment/color*))

(define-method (draw (actor <actor>))
  (disk *draw/actor/size*
        #:color *draw/actor/color*))

(define-method (draw-over obj base-pict)
  (let* ((junction-pict (draw obj))
         (x (- (get-pos-x obj)
               (/2 (pict-width junction-pict))))
         (y (- (get-pos-y obj)
               (/2 (pict-height junction-pict)))))
    (pin-over base-pict x y junction-pict)))

(define-method (draw-over (segment <road-segment>) base-pict)
  (let ((offset (exact->inexact (/ *draw/road-segment/width* 4)))
        (segment-pict (draw segment)))
    (pin-over base-pict offset offset segment-pict)))

(define-method (draw (world <world>))
  (define (draw-many-over objs base-pict)
    (fold draw-over base-pict objs))

  (define world-pict
    (remove-outline (rectangle (get world 'size-x)
                               (get world 'size-y))))
  (fold draw-many-over
        world-pict
        (list (get-road-segments world)
              (get-road-junctions world)
              (get-actors world))))

