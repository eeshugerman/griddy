(define-module (gritty util)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (->
            slot-push!
            zip-to-alist))


(define (slot-push! obj slot val)
  (let* ((old-list (slot-ref obj slot))
         (new-list (cons val old-list)))
    (slot-set! obj slot new-list)))

(define (-> obj . slots)
  ;; apparently `match' doesn't support tail patterns?
  ;; (match slots
  ;;   ((slots)
  ;;    (slot-ref obj slot))
  ;;   ((rest ... last)
  ;;    (slot-ref (-> obj rest ...) last)))
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
