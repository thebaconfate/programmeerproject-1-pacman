#lang r5rs

(#%require "constants.rkt" "position-adt.rkt" "direction-adt.rkt")
(#%provide make-moveable-adt)

(define (make-moveable-adt x y)
  (let ((position (make-position-adt x y))
        (direction (make-direction-adt)))

    (define (set-position! new-position)
      (set! position new-position))

    (define (get-direction)
      (direction 'get-direction))

    (define (set-direction! new-direction)
      ((direction 'set-direction!) new-direction))

    (define (reset-position!)
      (set! position (make-position-adt x y))
      (set! direction (make-direction-adt)))

    (define (invoke-proc-with-direction direction proc-up proc-down proc-right proc-left)
      (cond
        ((eq? direction up)(proc-up))
        ((eq? direction down)(proc-down))
        ((eq? direction right)(proc-right))
        ((eq? direction left)(proc-left))))

    (define position-up
      (lambda ()
        (let ((x (position 'get-x))
              (y (position 'get-y)))
          (make-position-adt x (- y 1)))))

    (define position-down
      (lambda ()
        (let ((x (position 'get-x))
              (y (position 'get-y)))
          (make-position-adt x (+ y 1)))))

    (define position-right
      (lambda ()
        (let ((x (position 'get-x))
              (y (position 'get-y)))
          (make-position-adt (+ x 1) y))))

    (define position-left
      (lambda ()
        (let ((x (position 'get-x))
              (y (position 'get-y)))
          (make-position-adt (- x 1) y))))

    (define (next-position direction)
      (invoke-proc-with-direction direction
                                  position-up
                                  position-down
                                  position-right
                                  position-left))

    (define (move!)
      (let* ((direction (get-direction))
             (next-position (next-position direction)))
        (set! position next-position)))

    (define (move-with-direction! direction)
      (let ((next-position (next-position direction)))
        (set! position next-position)))

    (define (teleport! x)
      (set! position (make-position-adt x (position 'get-y))))

    (define moveable-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-direction)(get-direction))
          ((eq? message 'set-direction!)set-direction!)
          ((eq? message 'reset-position!)(reset-position!))
          ((eq? message 'set-position!)set-position!)
          ((eq? message 'move!)(move!))
          ((eq? message 'next-position) next-position)
          ((eq? message 'teleport!) teleport!)
          ((eq? message 'move-with-direction!) move-with-direction!)
          (else (position message)))))
    moveable-dispatch))
