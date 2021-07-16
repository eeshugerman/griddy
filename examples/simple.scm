;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (pict)
             (griddy core)
             (griddy util)
             (griddy draw)
             (griddy simulate))

(for-each (compose reload-module resolve-module)
          '((griddy core)
            (griddy util)
            (griddy draw)
            (griddy simulate)))

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

         (lane-1 (make <road-lane> #:direction 'forw))
         (lane-2 (make <road-lane> #:direction 'back))
         (lane-3 (make <road-lane> #:direction 'forw)))

    (link! junction-1 segment-1 junction-2)
    (link! junction-1 segment-2 junction-3)

    (link! lane-1 segment-1)
    (link! lane-2 segment-2)
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
  (let* (
         (lane-1 (first (get-road-lanes world))) ;; forw
         (actor-1 (make <actor>))
         (location-1 (make <location> #:road-lane lane-1 #:pos-param 0.25))

         (lane-2 (second (get-road-lanes world))) ;; back
         (actor-2 (make <actor>))
         (location-2 (make <location> #:road-lane lane-2 #:pos-param 0.75))
         )

    ;; (link! actor-1 location-1)
    ;; (set-route actor-1 '((arrive-at 0.75)))

    (link! actor-2 location-2)
    (set-route actor-2 (make <route>
                         #:steps `((turn-onto ,lane-3)
                                   (arrive-at 0.5))))))

(define world (get-first make-skeleton add-actors!))
(pict->file (draw world) "foo-1.svg")
(set! world (iterate make-skeleton world))
(pict->file (draw world) "foo-2.svg")
