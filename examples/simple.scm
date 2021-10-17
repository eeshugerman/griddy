;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (griddy core)
             (griddy util)
             (griddy simulate))

(for-each (compose reload-module resolve-module)
          '((griddy core)
            (griddy util)
            (griddy draw)
            (griddy simulate)))

(define (make-skeleton)
  (let* ((world (make <world>))

         (junction-1 (make <road-junction> #:x 250 #:y 250)) ;; center
         (junction-2 (make <road-junction> #:x 250 #:y 400)) ;; mid top
         (junction-3 (make <road-junction> #:x 400 #:y 250)) ;; mid right

         (segment-1 (make <road-segment>)) ;; vertical
         (lane-1-1 (make <road-lane/segment> #:direction 'forw)) ;; downward

         (segment-2 (make <road-segment>)) ;; horizontal
         (lane-2-1 (make <road-lane/segment> #:direction 'back)) ;; leftward
         (lane-2-2 (make <road-lane/segment> #:direction 'forw)) ;; rightward

         (segment-3 (make <road-segment>)) ;; diagonal
         (lane-3-1 (make <road-lane/segment> #:direction 'back))
         (lane-3-2 (make <road-lane/segment> #:direction 'forw)))

    (link! junction-1 segment-1 junction-2)
    (link! junction-1 segment-2 junction-3)
    (link! junction-2 segment-3 junction-3)

    (link! lane-1-1 segment-1)

    (link! lane-2-1 segment-2)
    (link! lane-2-2 segment-2)

    (link! lane-3-1 segment-3)
    (link! lane-3-2 segment-3)

    (for-each (cut add! world <>)
              (list junction-1
                    junction-2
                    junction-3
                    segment-1
                    segment-2
                    segment-3
                    lane-1-1
                    lane-2-1
                    lane-2-2
                    lane-3-1
                    lane-3-2))
    world))

(define (add-actors! world)
  (let* ((lane-1-1 (fifth  (get-static-items world <road-lane>)))
         (lane-2-1 (fourth (get-static-items world <road-lane>)))
         (lane-2-2 (third  (get-static-items world <road-lane>)))

         (actor-1 (make <actor>))
         (location-1
          (make <location> #:road-lane lane-2-2 #:pos-param 0.25))
         (dest-1
          (make <location> #:road-lane lane-2-1 #:pos-param 0.75))

         (actor-2 (make <actor>))
         (location-2
          (make <location> #:road-lane lane-2-1 #:pos-param 0.5))
         (dest-2
          (make <location> #:road-lane lane-1-1 #:pos-param 0.75)))

    (link! actor-1 location-1)
    (agenda-push! actor-1 `(travel-to ,dest-1))

    (link! actor-2 location-2)
    (agenda-push! actor-2 `(travel-to ,dest-2))))

(simulate make-skeleton add-actors!)
