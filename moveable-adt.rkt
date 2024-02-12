#lang r5rs

(#%require "position-adt.rkt")
(#%provide make-moveable-adt)

(define (make-moveable-adt x y)
  (let ((position (make-position-adt x y))
        (direction 'up))

    (define (set-x! new-x)
      (set! position (make-position-adt new-x (position 'get-y))))

    (define (set-y! new-y)
      (set! position (make-position-adt (position 'get-x) new-y)))

    (define moveable? #t)

    (define (get-direction) direction)
    
    (define (set-direction! new-direction)
      (set! direction new-direction))

    (define (reset-position!)
      (set! position (make-position-adt x y)))
      
    (define (move!)
      (cond
        ((eq? 'up direction)(set-y! (+ (position 'get-y) 1)))
        ((eq? 'right direction)(set-x! (+ (position 'get-x) 1)))
        ((eq? 'left direction)(set-x! (- (position 'get-x) 1)))
        ((eq? 'down direction)(set-y! (- (position 'get-y) 1)))))

    (define moveable-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-direction)(get-direction))
          ((eq? message 'set-direction!)set-direction!)
          ((eq? message 'reset-position!)(reset-position!))
          ((eq? message 'move!)(move!))
          (else (position message)))))
    moveable-dispatch))
