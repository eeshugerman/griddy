(use-modules (oop goops)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-27)
             (griddy core)
             (griddy simulate))

(define (random-bool)
  (even? (random-integer 2)))

(define (make-skeleton)
  (let* ((world (make <world>))
         (n 5)
         (junctions (make-array *unspecified* n n))
         (segments (make-array *unspecified* 2 n n)))

    (array-index-map!
     junctions
     (lambda (i j)
       (let* ((t (lambda (x) (+ 50 (* x 150))))
              (junction
               (make <road-junction> #:x (t i) #:y (t j))))
         (add! world junction)
         junction)))

    (array-index-map!
     segments
     (lambda (i j k)
       (if (or (and (= i 0) (= k (- n 1)))
               (and (= i 1) (= j (- n 1))))
           #f
           (let* ((junction-1 (array-ref junctions j k))
                  (junction-2 (if (= i 0)
                                  (array-ref junctions j (+ k 1))
                                  (array-ref junctions (+ j 1) k)))
                  (segment (make <road-segment>))
                  (lane-1 (make <road-lane/segment> #:direction 'forw))
                  (lane-2 (make <road-lane/segment> #:direction 'back))
                  (lane-3 (make <road-lane/segment> #:direction 'forw))
                  (lane-4 (make <road-lane/segment> #:direction 'back)))

             (link! junction-1 segment junction-2)
             (link! lane-1 segment)
             (link! lane-2 segment)
             (add! world segment)
             (add! world lane-1)
             (add! world lane-2)

             (when (and (= i 0) (even? j))
               (let* ((up (= 0 (remainder j 4)))
                      (lane (if up lane-3 lane-4)))
                 (link! lane segment)
                 (add! world lane)))

             (when (and (= i 1) (even? k))
               (let* ((up (= 0 (remainder k 4)))
                      (lane (if up lane-3 lane-4)))
                 (link! lane segment)
                 (add! world lane)))))))

    (array-for-each (cut connect-by-rank! <> world) junctions)

    world))

(define (add-actors! world)
  (random-source-randomize! default-random-source)
  (let* ((actors       (list-tabulate 100 (lambda (_) (make <actor>))))
         (segments     (get-road-segments world))
         (num-segments (length segments))
         (random-location
          (lambda ()
            (let* ((segment     (list-ref segments (random-integer num-segments)))
                   (side        (if (random-bool) 'forw 'back))
                   (pos-param   ((if (random-bool) + -)
                                 1/2
                                 (* 1/4 (random-real)))))
              (make <location/off-road>
                #:road-segment segment
                #:road-side-direction side
                #:pos-param pos-param)))))
    (for-each
     (lambda (actor)
       (link! actor (random-location))
       (agenda-push! actor `(sleep-for ,(random-integer 500)))
       (agenda-push! actor `(travel-to ,(random-location))))
     actors)))

(simulate make-skeleton add-actors! #:width 700 #:height 700 #:duration 30)
