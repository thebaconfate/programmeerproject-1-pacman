#lang r5rs

(#%require "constants.rkt")
(#%require "draw-adt.rkt")

(#%provide make-game-adt)

(define (make-game-adt)
  (let* ((level 1)
         (level-adt '())
         (draw-adt (make-draw-adt window-width-px window-height-px)))


    (define dispatch
      (lambda (message)
        (cond
          (else "Unknown message"))))
    dispatch))