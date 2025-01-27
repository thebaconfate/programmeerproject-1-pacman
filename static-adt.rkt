#lang r5rs

(#%require "position-adt.rkt")
(#%provide make-static-adt)

(define (make-static-adt x y type)
  (let ((position (make-position-adt x y)))

    (define (draw! draw-adt)
      ((draw-adt 'draw!) dispatch))

    (define dispatch
      (lambda (message)
        (cond
          ((eq? message 'to-string) (symbol->string type))
          ((eq? message 'get-type) type)
          ((eq? message 'draw!) draw!)
          (else (position message) ))))
    dispatch))

