(define-module (gritty draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (gritty core)
  #:use-module (gritty math)
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
  (line (pos-x (start-junction segment))
        (pos-y (start-junction segment))
        (pos-x (stop-junction segment))
        (pos-y (stop-junction segment))
        #:stroke-width *draw/road-segment/width*
        #:color *draw/road-segment/color*))

(define-method (draw (actor <actor>))
  (disk *draw/actor/size*
        #:color *draw/actor/color*))

(define-method (draw-over (obj <point-like>) base-pict)
  (let* ((junction-pict (draw obj))
         (x (- (pos-x obj)
               (/2 (pict-width junction-pict))))
         (y (- (pos-y obj)
               (/2 (pict-height junction-pict)))))
    (pin-over base-pict x y junction-pict)))

(define-method (draw-over (segment <road-segment>) base-pict)
  (let ((offset (exact->inexact (/ *draw/road-segment/width* 4)))
        (segment-pict (draw segment)))
    (pin-over base-pict offset offset segment-pict)))

(define (draw-many-over objs base-pict)
  (fold draw-over base-pict objs))

(define-method (draw (world <world>))
  (let* ((world-pict
          (remove-outline (rectangle (size-x world)
                                     (size-y world))))
         (populated-world-pict
          (fold draw-many-over
                world-pict
                (list (road-segments world)
                      (road-junctions world)
                      (actors world)))))
    populated-world-pict))

