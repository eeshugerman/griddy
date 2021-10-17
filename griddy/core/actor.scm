(define-module (griddy core actor)
  #:duplicates (merge-generics)
  #:use-module (oop goops)
  #:use-module (griddy constants)
  #:use-module (griddy util)
  #:export (<actor>
            agenda-pop!
            agenda-push!
            route-pop!
            route-reset!))

(util:extend-primitives!)

(define-class <actor> ()
  location
  (max-speed
   #:init-keyword #:max-speed
   #:init-value *actor/speed*) ;; units / second
  (route ;; 'none or list
   ;; '() means end of route
   #:init-form 'none)
  (agenda
   #:init-thunk list))

(define-method (agenda-push! (actor <actor>) item)
  (extend! (ref actor 'agenda) item))

(define-method (agenda-pop! (actor <actor>))
  (let ((current-agenda (ref actor 'agenda)))
    (set! (ref actor 'agenda) (cdr current-agenda))
    (car current-agenda)))

(define-method (route-pop! (actor <actor>))
  (set! (ref actor 'route) (cdr (ref actor 'route))))

(define-method (route-reset! (actor <actor>))
  (set! (ref actor 'route) 'none))
