;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (srfi srfi-26)
             (gritty core)
             (gritty draw))

(define world (make <world>))

(let ((j1 (make <road-junction> #:x 0 #:y 500))
      (j2 (make <road-junction> #:x 500 #:y 0)))
  (add! world j1)
  (add! world j2))

(let* ((j1 (make <road-junction> #:x 250 #:y 250))
       (j2 (make <road-junction> #:x 250 #:y 400))
       (j3 (make <road-junction> #:x 400 #:y 250))

       (s1 (make <road-segment>))
       (s2 (make <road-segment>))

       (l1 (make <road-lane>))
       (l2 (make <road-lane>))
       (l3 (make <road-lane>))

       (a1 (make <actor>)))

  (link! j1 s1 j2)
  (link! j1 s2 j3)

  (link! l1 s1 'forward)
  (link! l2 s1 'backward)
  (link! l3 s2 'forward)

  (link! a1 l3 0.5)

  (add! world j1)
  (add! world j2)
  (add! world j3)

  (add! world s1)
  (add! world s2)

  (add! world a1)

  (for-each (cut add! world <>)
            (list
             j1
             j2
             j3
             s1
             s2
             l1
             l2
             l3
             a1)))

(draw world)
