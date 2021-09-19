(define-module (griddy util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:replace (set!)
  #:export (get
            extend
            extend!
            insert!
            zip-to-alist))


(define (extend list' . items)
  (append list' items))

(define (get obj . slots)
  (define (poly-ref obj' key)
    (if (list? obj')
        (cdr (assq key obj'))
        (slot-ref obj' key)))

  ;; (match slots
  ;;   ((slot)
  ;;    (poly-ref obj slot))
  ;;   ((but-last ..1 last)
  ;;    (poly-ref (get obj but-last ..1) last)))

  (define (but-last list') (drop-right list' 1))
  (cond
   ((= 1 (length slots))
    (poly-ref obj (car slots)))
   (else
    (poly-ref (apply get (cons obj (but-last slots)))
              (last slots)))))

(define (poly-set! obj key val)
  (if (list? obj)
      (assoc-set! obj key val)
      (slot-set! obj key val)))

(define-syntax set!
  (syntax-rules (get)
    ((_ (get obj slot) val)
     (poly-set! obj slot val))
    ((_ (get obj slots ... slot ) val)
     (poly-set! (get obj slots ...) slot val))
    ((_ var val)
     ((@ (guile) set!) var val))))

(define-syntax insert!
  (syntax-rules (get)
    ((_ (get obj slot) val)
     (poly-set! obj
                slot
                (cons val (get obj slot))))
    ((_ (get obj slots ... slot) val)
     (poly-set! (get obj slots ...)
                slot
                (cons val (get obj slots ... slot))))
    ((_ list' val)
     (set! list' (cons val list')))))

(define-syntax extend!
  (syntax-rules (get)
    ((_ list' vals ...)
     (set! list' (extend list' vals ...)))
    ((_ (get obj slot) vals ...)
     (poly-set! obj
                slot
                (extend (get obj slot) vals ...)))
    ((_ (get obj slots ... slot) vals ...)
     (poly-set! (get obj slots ...)
                slot
                (extend (get obj slots ... slot) vals ...)))))


(define (zip-to-alist list-1 list-2)
  (let loop ((acc '())
             (list-1* list-1)
             (list-2* list-2))
    (match (cons list-1* list-2*)
      ((() . ())
       acc)
      ((or (() . (_ ...)) ((_ ...) . ()))
       (throw 'list-have-different-lengths))
      (((head-1 tail-1 ...) . (head-2 tail-2 ...))
       (loop (cons (cons head-1 head-2) acc)
             tail-1
             tail-2)))))
