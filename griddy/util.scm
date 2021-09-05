(define-module (griddy util)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (get
            append-1
            slot-add!
            slot-append!
            zip-to-alist))


(define (append-1 l item)
  (append l (list item)))

(define-method (add-to (container <list>) val)
  (cons val container))

(define-method (append-to (container <list>) val)
  (append-1 container val))

(define (slot-add! obj slot val)
  (let* ((old-container (slot-ref obj slot))
         (new-container (add-to old-container val)))
    (slot-set! obj slot new-container)))

(define (slot-append! obj slot val)
  (let* ((old-container (slot-ref obj slot))
         (new-container (append-to old-container val)))
    (slot-set! obj slot new-container)))

(define (get obj . slots)
  ;; apparently `match' doesn't support tail patterns?
  ;; (match slots
  ;;   ((slots)
  ;;    (slot-ref obj slot))
  ;;   ((rest ... last)
  ;;    (slot-ref (get obj rest ...) last)))
  (define (but-last l) (drop-right l 1))
  (cond
   ((= 1 (length slots))
    (slot-ref obj (car slots)))
   (else
    (slot-ref (apply get (cons obj (but-last slots)))
              (last slots)))))


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
