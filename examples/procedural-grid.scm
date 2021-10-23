(use-modules (oop goops)
             (pipe)
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
         (junctions (make-array *unspecified* n n)))

    (array-index-map!
     junctions
     (lambda (i j)
       (let* ((t (lambda (x) (+ 50 (* x 150))))
              (junction
               (make <road-junction> #:x (t i) #:y (t j))))
         (add! world junction)
         junction)))

    (define (add-segments! dim i j)
      (if (or (and (eq? dim 'i) (= j (- n 1)))
              (and (eq? dim 'j) (= i (- n 1))))
          #f
          (let* ((junction-1 (array-ref junctions i j))
                 (junction-2 (if (eq? dim 'i)
                                 (array-ref junctions i (+ j 1))
                                 (array-ref junctions (+ i 1) j)))
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

            (let* ((dim-val (match dim ('i i) ('j j)))
                   (up      (= 0 (remainder dim-val 4)))
                   (lane    (if up lane-3 lane-4)))
              (when (even? dim-val)
                (link! lane segment)
                (add! world lane))))))
    (for-each
     (lambda (dim)
       (for-each
        (lambda (i)
          (for-each
           (lambda (j)
             (add-segments! dim i j))
           (iota n)))
        (iota n)))
     '(i j))

    (array-for-each (cut connect-by-rank! <> world) junctions)

    world))

(define (add-actors! world)
  (random-source-randomize! default-random-source)
  (let* ((make-actor
          (lambda (_)
            (make <actor> #:max-speed (+ 30 (* (random-integer 30))))))
         (actors
          (list-tabulate 100 make-actor))
         (segments
          (list->array 1 (get-static-items world <road-segment>)))
         (random-location
          (lambda ()
            (let* ((segment   (->> segments
                                   (array-length)
                                   (random-integer)
                                   (array-ref segments)))
                   (side      (if (random-bool) 'forw 'back))
                   (pos-param (+ 1/4 (* 1/2 (random-real)))))
              (make <location/off-road>
                #:road-segment segment
                #:road-side-direction side
                #:pos-param pos-param)))))
    (for-each
     (lambda (actor)
       (link! actor (random-location))
       (agenda-push! actor `(sleep-for ,(random-integer 300)))
       (agenda-push! actor `(travel-to ,(random-location))))
     actors)))

(simulate make-skeleton add-actors! #:width 700 #:height 700 #:duration 60)
