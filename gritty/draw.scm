(define-module (gritty draw)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pict)
  #:use-module (gritty core)
  #:export (draw))

(define *road-junction-size* 10)
(define *frame-width* 10)

(define-method (draw (w <world>))
  (define (draw-item-over item base-pict)
    (pin-over base-pict
              (pos-x item)
              (pos-y item)
              (draw item)))

  (let* ((world-pict (rectangle (size-x w) (size-y w)))
         (populated-world-pict
          (fold draw-item-over world-pict (road-junctions w))))
    populated-world-pict))

(define-method (draw (j <road-junction>))
  (disk *road-junction-size*))
