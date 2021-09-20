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
                 (cddr defs)))))))

(define-constants
  *road-junction/size*         25
  *road-junction/color*        tango-aluminium-6

  *road-segment/color*         tango-aluminium-5
  *road-segment/wiggle-room-%* 5

  *road-lane/arrow-size*       5
  *road-lane/color*            tango-plum
  *road-lane/width*            10

  *actor/size*                 5
  *actor/color*                tango-light-chameleon
  )
