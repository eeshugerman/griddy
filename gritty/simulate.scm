(define-module (gritty simulate)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (gritty core)
  #:export (iterate))

(define-method (advance (junction <road-junction>))
  )

(define-method (advance! (world <world>))
  ;; strategy:
  ;; - static objects (<road-*>, etc) will be reused, mutated
  ;; - for each actor container (eg (-> <road-lane> 'actors)),
  ;;   create an empty `actors-next' container friend
  ;; - loop over actors, populate `actors-next' containers with
  ;;   `actor-next's
  ;; - replace old containers with `foo-next' containers
  (let* ((actor-containers
          (map (cut -> <> 'actors) (get-lanes world)))
         (actor-containers-next
          (map (lambda (_) (make-bbtree)) actor-containers))
         (actor-containers-map
          ;; or what-have-you
          (make-hash-table (actor-containers . actor-containers-next)))
    (for-each
     (lambda (actor-container)
       ...
       (for-each
        (lambda (actor)
          ...)
        actor-container))
     actor-containers))))
