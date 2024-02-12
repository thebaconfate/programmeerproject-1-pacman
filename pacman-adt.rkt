#lang r5rs

(#%require "moveable-adt.rkt")
(#%provide make-pacman-adt)


(define (make-pacman-adt x y)
  (let ((moveable (make-moveable-adt x y)))

    (define type 'pacman)

    (define (draw! draw-adt)
      ((draw-adt 'draw-pacman!) pacman-dispatch))

    (define pacman-dispatch
      (lambda (message)
        (cond
          ((eq? message 'draw!)draw!)
          ((eq? message 'type) type)
          (else (moveable message)))))
    pacman-dispatch))