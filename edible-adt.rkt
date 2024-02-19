#lang r5rs

(#%require "static-adt.rkt")
(#%provide make-edible-adt)

(define (make-edible-adt x y points type)
  (let ((static-adt (make-static-adt x y type)))


    (define draw!
      (lambda (draw-adt)
        ((draw-adt 'draw-edible!) edible-dispatch)))

    (define edible-dispatch
      (lambda (message)
        (cond
          ((eq? message 'draw!) draw!)
          ((eq? message 'points) points)
          (else (static-adt message)))))
    edible-dispatch))