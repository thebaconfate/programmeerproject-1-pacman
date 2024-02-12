#lang r5rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               position ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Author: Gérard Lichtert
;; you can make a position-adt with (make-position-adt number number) -> position-adt
;; you can get the x coordinate with (position-adt 'get-x) -> number
;; you can get the y coordinate with (position-adt 'get-y) -> number
;; you can check if two positions are equal with ((position-adt '=position) position-adt) -> boolean
(#%provide make-position-adt)

(define (make-position-adt x y)
  
  (define (get-x) x)
  
  (define (get-y) y)

  (define (=position other-position)
    (and (= (get-x)(other-position 'get-x))
         (= (get-y)(other-position 'get-y))))
  
  (define position-dispatch
    (lambda(message)
      (cond
        ((eq? message 'get-x)(get-x))
        ((eq? message 'get-y)(get-y))
        ((eq? message '=position)=position)
        (else "ERROR - Method not recognized by POSITION ADT "))))
  position-dispatch)
