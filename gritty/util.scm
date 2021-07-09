(define-module (gritty util)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (pfds queues)
  #:use-module (ice-9 match)
  #:export (get
            slot-push!
            zip-to-alist))

;; is this the best option?
(define <queue> (class-of (make-queue)))

(define-method (add-to (container <list>) val)
  (cons val container))

(define-method (add-to (container <queue>) val)
  (enqueue container val))

(define (slot-push! obj slot val)
  (let* ((old-container (slot-ref obj slot))
         (new-container (add-to old-container val)))
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
    (slot-ref (apply -> (cons obj (but-last slots)))
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
