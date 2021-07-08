(define-module (gritty draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (gritty util)
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
  (line (-> segment 'start-junction 'pos-x)
        (-> segment 'start-junction 'pos-y)
        (-> segment 'stop-junction 'pos-x)
        (-> segment 'stop-junction 'pos-y)
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

(define (draw-many-over objs base-pict)
  (fold draw-over base-pict objs))

(define-method (draw (world <world>))
  (let* ((world-pict
          (remove-outline (rectangle (-> world 'size-x)
                                     (-> world 'size-y))))
         (populated-world-pict
          (fold draw-many-over
                world-pict
                (list (get-road-segments world)
                      (get-road-junctions world)
                      (get-actors world)))))
    populated-world-pict))
