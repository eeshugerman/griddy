;;; GUILE_LOAD_PATH="$(pwd)" guile examples/procedural.scm

(use-modules (oop goops)
             (ice-9 match)
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
         (n 4)
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
                  (lane-1 (make <road-lane> #:direction 'forw))
                  (lane-2 (make <road-lane> #:direction 'back)))
             (link! junction-1 segment junction-2)
             (link! lane-1 segment)
             (link! lane-2 segment)
             (add! world segment)
             (add! world lane-1)
             (add! world lane-2)))))

    world))

(define (add-actors! world)
  #t)

(simulate make-skeleton add-actors! #:width 550 #:height 550)
