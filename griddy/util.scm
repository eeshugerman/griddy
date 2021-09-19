(define-module (griddy util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:replace (set!)
  #:export (ref
            match-direction
            extend
            extend!
            insert!))

(define (extend list' . items)
  (append list' items))

(define (ref obj . slots)
  (define (poly-ref obj' key)
    (if (list? obj')
        (assq-ref obj' key)
        (slot-ref obj' key)))
  (match slots
    ((slot)
     (poly-ref obj slot))
    ((slots ... slot)
     (poly-ref (apply ref (cons obj slots))
               slot))))

(define (poly-set! obj key val)
  (if (list? obj)
      (assoc-set! obj key val)
      (slot-set! obj key val)))

(define-syntax set!
  (syntax-rules (ref)
    ((_ (ref obj slot) val)
     (poly-set! obj slot val))
    ((_ (ref obj slots ... slot ) val)
     (poly-set! (ref obj slots ...) slot val))
    ((_ var val)
     ((@ (guile) set!) var val))))

(define-syntax insert!
  (syntax-rules (ref)
    ((_ (ref obj slot) val)
     (poly-set! obj
                slot
                (cons val (ref obj slot))))
    ((_ (ref obj slots ... slot) val)
     (poly-set! (ref obj slots ...)
                slot
                (cons val (ref obj slots ... slot))))
    ((_ list' val)
     (set! list' (cons val list')))))

(define-syntax extend!
  (syntax-rules (ref)
    ((_ list' vals ...)
     (set! list' (extend list' vals ...)))
    ((_ (ref obj slot) vals ...)
     (poly-set! obj
                slot
                (extend (ref obj slot) vals ...)))
    ((_ (ref obj slots ... slot) vals ...)
     (poly-set! (ref obj slots ...)
                slot
                (extend (ref obj slots ... slot) vals ...)))))

(define-syntax-rule (match-direction lane if-forw if-back)
  (case (ref lane 'direction)
    ((forw) if-forw)
    ((back) if-back)))
