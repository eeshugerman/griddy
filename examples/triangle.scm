(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (griddy core)
             (griddy util)
             (griddy simulate))

(define (make-skeleton)
  (let* ((world (make <world>))
         (junction-1 (make <road-junction> #:x 100 #:y 100))
         (junction-2 (make <road-junction> #:x 400 #:y 100))
         (junction-3 (make <road-junction> #:x 250 #:y 400))
         (segment-1 (make <road-segment>))
         (segment-2 (make <road-segment>))
         (segment-3 (make <road-segment>))
         (lane-1 (make <road-lane/segment> #:direction 'forw))
         (lane-2 (make <road-lane/segment> #:direction 'forw))
         (lane-3 (make <road-lane/segment> #:direction 'forw)))

    (link! lane-1 segment-1)
    (link! lane-2 segment-2)
    (link! lane-3 segment-3)

    (link! junction-1 segment-1 junction-2)
    (link! junction-2 segment-2 junction-3)
    (link! junction-3 segment-3 junction-1)

    (for-each (cut add! world <>)
              (list junction-1
                    junction-2
                    junction-3
                    segment-1
                    segment-2
                    segment-3
                    lane-1
                    lane-2
                    lane-3))

    (for-each (cut connect-by-rank! <> world)
              (list junction-1 junction-2 junction-3))
    world))

(define (add-actors! world)
  (let* ((segment (car  (get-static-items world <road-segment>)))
         (actor (make <actor>))
         (init-location
          (make <location/off-road>
            #:road-segment segment
            #:road-side-direction 'forw
            #:pos-param 0.25))
         (dest-location
          (make <location/off-road>
            #:road-segment segment
            #:road-side-direction 'forw
            #:pos-param 0.75)))
    (link! actor init-location)
    (agenda-push! actor `(travel-to ,dest-location))))

(simulate make-skeleton add-actors!)
