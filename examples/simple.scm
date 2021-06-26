;;; GUILE_LOAD_PATH="$(pwd)" guile examples/simple.scm

(use-modules (oop goops)
             (gritty core))

(define world (make <world>))

(let ((j1 (make <road-junction> #:x 0 #:y 0))
      (j2 (make <road-junction> #:x 0 #:y 9))
      (s (make <road-segment>)))
  (link! j1 s j2)
  (add! world j1)
  (add! world j2)
  (add! world s))

(display world)
(newline)
