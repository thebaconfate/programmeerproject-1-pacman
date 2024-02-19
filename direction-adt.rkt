#lang r5rs


(#%require "constants.rkt")
(#%provide make-direction-adt)

(define (make-direction-adt)
  (let ((directions (vector up right left down))
        (direction 0))

    (define get-direction
      (lambda ()
        (vector-ref directions direction)))
    (define raw-set-direction!
      (lambda
          (new-direction)
        (set! direction new-direction)))
    (define set-and-get-direction!
      (lambda (new-direction)
        (begin
          (raw-set-direction! new-direction)
          (get-direction))))

    (define rotate-clockwise!
      (lambda ()
        (set-and-get-direction! (modulo (+ direction 1) 4))))

    (define rotate-counter-clockwise!
      (lambda ()
        (set-and-get-direction! (modulo (- direction 1) 4))))

    (define rotate-180!
      (lambda ()
        (set-and-get-direction! (modulo (+ direction 2) 4))))

    (define set-direction!
      (lambda (new-direction)
        (cond
          ((eq? new-direction up)(set-and-get-direction! 0))
          ((eq? new-direction right)(set-and-get-direction! 1))
          ((eq? new-direction left)(set-and-get-direction! 2))
          ((eq? new-direction down)(set-and-get-direction! 3)))))

    (define direction-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-direction) (get-direction))
          ((eq? message 'set-direction!) set-direction!)
          ((eq? message 'rotate-clockwise!) (rotate-clockwise!))
          ((eq? message 'rotate-counter-clockwise!) (rotate-counter-clockwise!))
          ((eq? message 'rotate-180!) (rotate-180!)))))
    direction-dispatch))