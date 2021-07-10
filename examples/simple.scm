;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (srfi srfi-26)
             (pict)
             (gritty core)
             (gritty util)
             (gritty draw)
             (gritty simulate))

(for-each (compose reload-module resolve-module)
          '((gritty core)
            (gritty util)
            (gritty draw)
            (gritty simulate)))

(define (make-skeleton)
  (define world (make <world>))

  (let ((junction-1 (make <road-junction> #:x 0 #:y 500))
        (junction-2 (make <road-junction> #:x 500 #:y 0)))
    (add! world junction-1)
    (add! world junction-2))

  (let* ((junction-1 (make <road-junction> #:x 250 #:y 250))
         (junction-2 (make <road-junction> #:x 250 #:y 400))
         (junction-3 (make <road-junction> #:x 400 #:y 250))

         (segment-1 (make <road-segment>))
         (segment-2 (make <road-segment>))

         (lane-1 (make <road-lane> #:direction 'forward))
         (lane-2 (make <road-lane> #:direction 'backward))
         (lane-3 (make <road-lane> #:direction 'forward)))

    (link! junction-1 segment-1 junction-2)
    (link! junction-1 segment-2 junction-3)

    (link! lane-1 segment-1)
    (link! lane-2 segment-1)
    (link! lane-3 segment-2)

    (for-each (cut add! world <>)
              (list junction-1
                    junction-2
                    junction-3
                    segment-1
                    segment-2
                    lane-1
                    lane-2
                    lane-3)))
  world)

(define (add-actors! world)
  (let* ((lane (first (get-road-lanes world)))
         (location (make <location>
                     #:road-lane lane
                     #:pos-param 0.25))
         (actor (make <actor> #:max-speed 25)))
    (set-route actor '((arrive-at 0.75)))
    (link! actor location)))

(define world (get-first make-skeleton add-actors!))
;; (draw world)
;; (pict->file (draw world) "foo-1.svg")
;; (set! world (get-next make-skeleton world))
;; (draw world)
;; (pict->file (draw world) "foo-2.svg")
