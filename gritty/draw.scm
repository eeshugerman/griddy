(define-module (gritty draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (gritty core)
  #:use-module (gritty math)
  #:export (draw))

(define *draw/road-junction/size* 50)
(define *draw/road-junction/color* "grey")
(define *draw/road-segment/width* 40)

(define (square size)
  (rectangle size size))

(define-method (draw (j <road-junction>))
  (fill (square *draw/road-junction/size*)
        *draw/road-junction/color*))

(define-method (draw (s <road-segment>))
  (line (pos-x (start-junction s))
        (pos-y (start-junction s))
        (pos-x (stop-junction s))
        (pos-y (stop-junction s))
        #:stroke-width *draw/road-segment/width*))

(define-method (draw-over (j <road-junction>) base-pict)
  (let* ((junction-pict (draw j))
         (x (- (pos-x j)
               (/2 (pict-width junction-pict))))
         (y (- (pos-y j)
               (/2 (pict-height junction-pict)))))
    (pin-over base-pict x y junction-pict)))

(define-method (draw-over (s <road-segment>) base-pict)
  (let ((offset (exact->inexact (/ *draw/road-segment/width* 4)))
        (segment-pict (draw s)))
    (pin-over base-pict offset offset segment-pict)))

(define (draw-many-over items base-pict)
  (fold draw-over base-pict items))

(define-method (draw (w <world>))
  (let* ((world-pict
          (remove-outline (rectangle (size-x w) (size-y w))))
         (populated-world-pict
          (fold draw-many-over
                world-pict
                (list (road-segments w)
                      (road-junctions w)))))
    populated-world-pict))

