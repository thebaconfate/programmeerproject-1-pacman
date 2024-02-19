#lang r5rs

(#%require "constants.rkt" "position-adt.rkt" "direction-adt.rkt")
(#%provide make-moveable-adt)

(define (make-moveable-adt x y)
  (let ((position (make-position-adt x y))
        (direction (make-direction-adt)))

    (define (set-position-x! new-x)
      (let ((y (position 'get-y)))
        (set! position (make-position-adt new-x y))))

    (define (set-position-y! new-y)
      (let ((x (position 'get-x)))
        (set! position (make-position-adt x new-y))))

    (define (set-position! new-x new-y)
      (set! position (make-position-adt x y)))

    (define moveable? #t)

    (define (get-direction)
      (direction 'get-direction))

    (define (set-direction! new-direction)
      ((direction 'set-direction!) new-direction))

    (define (reset-position!)
      (set! position (make-position-adt x y))
      (set! direction (make-direction-adt)))

    (define (move!)
      (let ((direction (get-direction))
            (y (position 'get-y))
            (x (position 'get-x)))
        (cond
          ((eq? up direction)(set-position-y! (+ y 1)))
          ((eq? right direction)(set-position-x! (+ x 1)))
          ((eq? left direction)(set-position-x! (- x 1)))
          ((eq? down direction)(set-position-y! (- x 1))))))

    (define (move-with-direction! direction)
      (let ((y (position 'get-y))
            (x (position 'get-x)))
        (begin
          (set-direction! direction)
          (cond
            ((eq? up direction)(set-position-y! (+ y 1)))
            ((eq? right direction)(set-position-x! (+ x 1)))
            ((eq? left direction)(set-position-x! (- x 1)))
            ((eq? down direction)(set-position-y! (- x 1)))))))

    (define moveable-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-direction)(get-direction))
          ((eq? message 'set-direction!)set-direction!)
          ((eq? message 'reset-position!)(reset-position!))
          ((eq? message 'move!)(move!))
          ((eq? message 'move-with-direction!) move-with-direction!)
          (else (position message)))))
    moveable-dispatch))
