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

(let ((j1 (make <road-junction> #:x 50 #:y 50)))
  (add! world j1))

(draw world)
