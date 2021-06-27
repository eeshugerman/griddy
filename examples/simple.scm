;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (gritty core)
             (gritty draw))

(define world (make <world>))

(let ((j1 (make <road-junction> #:x 0 #:y 500))
      (j2 (make <road-junction> #:x 500 #:y 0)))
  (add! world j1)
  (add! world j2))

(let ((j1 (make <road-junction> #:x 250 #:y 250))
      (j2 (make <road-junction> #:x 250 #:y 400))
      (j3 (make <road-junction> #:x 400 #:y 250))
      (s1 (make <road-segment>))
      (s2 (make <road-segment>)))
  (link! j1 s1 j2)
  (link! j1 s2 j3)
  (add! world j1)
  (add! world j2)
  (add! world j3)
  (add! world s1)
  (add! world s2))

(draw world)
