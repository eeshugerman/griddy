(define-module (griddy constants)
  #:use-module (srfi srfi-1)
  #:use-module (chickadee graphics color))

(define-macro (define-constants . all-defs)
  (reverse
   (let loop ((form '(begin))
              (defs all-defs))
     (if (null? defs)
         form
         (let ((name (car defs))
               (val (cadr defs)))
           (loop (cons `(define-public ,name ,val) form)
                 (cddr defs))))
     ;; (match defs
     ;;   (()
     ;;    form)
     ;;   ((name val defs-rest ...)
     ;;    (loop (cons `(define-public ,name ,val) form)
     ;;          defs-rest)))
     )))

(define-constants
  *road-junction/color*        tango-aluminium-2

  *road-segment/color*         white
  *road-segment/wiggle-room-%* 20

  *road-lane/arrow-size*       5
  *road-lane/color*            tango-plum
  *road-lane/width*            15
  *road-lane/approx-pts*       5

  *actor/size*                 5
  *actor/color*                tango-light-chameleon

  *simulate/time-step*         1/10

  )
