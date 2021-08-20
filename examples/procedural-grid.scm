;;; GUILE_LOAD_PATH="$(pwd)" guile examples/procedural.scm

(use-modules (oop goops)
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
         (junctions (make-array *unspecified* 10 10)))

    (array-index-map!
     junctions
     (lambda (i j)
       (make <road-junction> #:x (* i 100) #:y (* j 100))))

    (array-for-each (cut add! world <>) junctions)
    world))

(define (add-actors! world)
  #t)

(simulate make-skeleton add-actors!)
