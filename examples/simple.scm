;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (gritty core)
             (gritty draw))

(define world (make <world>))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 0 #:y 100))
      (s (make <road-segment>)))
  (link! j1 s j2)
  (add! world j1)
  (add! world j2)
  (add! world s))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 100 #:y 0))
      (s (make <road-segment>)))
  (link! j1 s j2)
  (add! world j1)
  (add! world j2)
  (add! world s))

(let ((j1 (make <road-junction> #:x 30 #:y 30))
      (j2 (make <road-junction> #:x 60 #:y 60)))
  (add! world j1)
  (add! world j2))

(draw world)
