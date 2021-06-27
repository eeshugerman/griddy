(define-module (gritty draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (gritty core)
  #:use-module (gritty math)
  #:export (draw))

(define *draw/road-junction/size* 10)
(define *draw/road-junction/color* "grey")
(define *draw/road-segment/width* 5)

(define (square size)
  (rectangle size size))

(define-method (draw (w <world>))
  (let* ((world-pict
          (remove-outline (rectangle (size-x w) (size-y w))))
         (populated-world-pict
          (fold draw-many-over
                world-pict
                (list (road-junctions w)
                      (road-segments w)))))
    populated-world-pict))

(define (draw-many-over items base-pict)
  (fold draw-over base-pict items))

(define-method (draw (j <road-junction>))
  (fill (square *draw/road-junction/size*)
        *draw/road-junction/color*))

(define-method (draw-over (item <road-junction>) base-pict)
  (let* ((item-pict (draw item))
         (x (- (pos-x item)
               (/2 (pict-width item-pict))))
         (y (- (pos-y item)
               (/2 (pict-height item-pict)))))
    (pin-over base-pict x y (draw item))))

(define-method (draw (s <road-segment>))
  (line (pos-x (start-junction s))
        (pos-y (start-junction s))
        (pos-x (stop-junction s))
        (pos-y (stop-junction s))
        #:stroke-width *draw/road-segment/width*))

(define-method (draw-over (item <road-segment>) base-pict)
  (pin-over base-pict 0 0 (draw item)))

