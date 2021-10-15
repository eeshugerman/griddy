(define-module (griddy util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:replace (set!)
  #:export (extend
            extend!
            flip
            insert!
            match-direction
            neq?
            ref
            ref/default))

(define neq? (negate eq?))

(define (flip x)
  (case x
    ((forw) 'back)
    ((back) 'forw)
    ((beg)  'end)
    ((end)  'beg)))

(define (extend list' . items)
  (append list' items))

(define (poly-ref obj key)
  (cond
   ((list? obj)
    (assq-ref obj key))
   ((hash-table? obj)
    (hash-table-ref obj key))
   (else
    (slot-ref obj key))))

(define (poly-ref/default obj key default)
  (cond
   ((list? obj)
    (or (assq-ref obj key) default))
   ((hash-table? obj)
    (hash-table-ref/default obj key default))
   (else
    (if (slot-bound? obj key)
        (slot-ref obj key)
        default))))

(define (ref obj . slots)
  (match slots
    ((slot)
     (poly-ref obj slot))
    ((slots ... slot)
     (poly-ref (apply ref (cons obj slots))
               slot))))

(define (ref/default . args)
  (match args
    ((obj slot default)
     (poly-ref/default obj slot default))
    ((obj slots ... slot default)
     (poly-ref/default (apply ref (cons obj slots))
                       slot
                       default))))

(define (poly-set! obj key val)
  (cond
   ((list? obj)
    (assoc-set! obj key val))
   ((hash-table? obj)
    (hash-table-set! obj key val))
   (else
    (slot-set! obj key val))))

(define-syntax set!
  (syntax-rules (ref)
    ((_ (ref obj slot) val)
     (poly-set! obj slot val))
    ((_ (ref obj slots ... slot) val)
     (poly-set! (ref obj slots ...) slot val))
    ((_ var val)
     ((@ (guile) set!) var val))))

(define-syntax insert!
  (syntax-rules (ref)
    ((_ (ref obj slot) val)
     (poly-set! obj
                slot
                (cons val (ref/default obj slot '()))))
    ((_ (ref obj slots ... slot) val)
     (poly-set! (ref obj slots ...)
                slot
                (cons val (ref/default obj slots ... slot '()))))
    ((_ list' val)
     (set! list' (cons val list')))))

(define-syntax extend!
  (syntax-rules (ref)
    ((_ list' vals ...)
     (set! list' (extend list' vals ...)))
    ((_ (ref obj slot) vals ...)
     (poly-set! obj
                slot
                (extend (ref/default obj slot '()) vals ...)))
    ((_ (ref obj slots ... slot) vals ...)
     (poly-set! (ref obj slots ...)
                slot
                (extend (ref/default obj slots ... slot '()) vals ...)))))

(define-syntax-rule (match-direction lane if-forw if-back)
  (case (ref lane 'direction)
    ((forw) if-forw)
    ((back) if-back)))
