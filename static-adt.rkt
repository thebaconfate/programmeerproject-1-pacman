#lang r5rs

(#%require "position-adt.rkt")
(#%provide make-static-adt)

(define (make-static-adt x y type)
  (let ((position (make-position-adt x y)))


    (define draw!
      (lambda (draw-adt)
        ((draw-adt 'draw-static!) static-dispatch)))

    (define static-dispatch
      (lambda (message)
        (cond
          ((eq? message 'type) type)
          ((eq? message 'draw!) draw!)
          (else (position message)))))
    static-dispatch))

