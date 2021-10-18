(use-modules (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (griddy core)
             (griddy util)
             (griddy simulate))

(define (make-skeleton)
  (let* ((world (make <world>))
         (junction-1 (make <road-junction> #:x 100 #:y 250))
         (junction-2 (make <road-junction> #:x 400 #:y 250))
         (segment (make <road-segment>))
         (lane (make <road-lane/segment> #:direction 'forw)))
    (link! lane segment)
    (link! junction-1 segment junction-2)
    (for-each (cut add! world <>)
              (list junction-1
                    junction-2
                    segment
                    lane))
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
