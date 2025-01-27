#lang r5rs

(#%require "help-procedures.rkt" "position-adt.rkt")
(#%provide make-moveable-adt)

(define (make-moveable-adt x y type)
  (let ((position (make-position-adt x y))
        (rotate #f)
        (direction 'left))

    (define (set-x! new-x)
      (set! position (make-position-adt new-x (position 'get-y))))

    (define (set-y! new-y)
      (set! position (make-position-adt (position 'get-x) new-y)))



    (define (set-direction! new-direction)
      (case new-direction
        ((up down right left)(set! direction new-direction))
        (else (display-invalid-message new-direction "MOVEABLE_ADT -> Invalid direction"))))

    (define (reset-position!)
      (set! position (make-position-adt x y)))

    (define (move!)
      (case direction
        ((down)(set-y! (+ (position 'get-y) 1)))
        ((right)(set-x! (+ (position 'get-x) 1)))
        ((left)(set-x! (- (position 'get-x) 1)))
        ((up)(set-y! (- (position 'get-y)1)))))

    (define (draw! draw-adt)
      ((draw-adt 'draw!) moveable-dispatch))

    (define moveable-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-direction) direction)
          ((eq? message 'set-direction!) set-direction!)
          ((eq? message 'reset-position!) reset-position!)
          ((eq? message 'move!) move!)
          ((eq? message 'get-type) type)
          ((eq? message 'draw!) draw!)
          (else (position message)))))
    moveable-dispatch))
