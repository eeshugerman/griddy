(define-module (griddy core world)
  #:duplicates (merge-generics)
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-26)
  #:use-module (griddy util)
  #:export(<world>
           <static-item>
           <actor-container>
           get-actors
           add!
           get-static-items))

(define-class <static-item> ())
(define-class <actor-container> ())

(define-class <world> ()
  (static-items ;; roads, etc
   #:init-thunk list))

(define-method (add! (world <world>) (item <static-item>))
  (insert! (ref world 'static-items) item))

(define-method (get-actors (world <world>))
  (define (into-actors container actors)
    (append actors (get-actors container)))
  (fold into-actors
        (list)
        (filter (cut is-a? <> <actor-container>)
                (ref world 'static-items))))

(define-method (get-static-items (world <world>))
  (ref world 'static-items))

(define-method (get-static-items (world <world>) (type <class>))
  (filter (cut is-a? <> type)
          (ref world 'static-items)))

