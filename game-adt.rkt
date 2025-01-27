#lang r5rs

(#%require "constants.rkt" "help-procedures.rkt" "draw-adt.rkt" "level-adt.rkt")
(#%provide make-game-adt)

(define (make-game-adt)
  (let* ((level 1)
         (level-adt (make-level-adt true-game-width true-game-height level))
         (draw-adt (make-draw-adt window-width-px window-height-px)))

    (define (start)
      ((level-adt 'draw!) draw-adt))

    (define dispatch
      (lambda (message)
        (cond
          ((eq? message 'start) start)
          (else (display-invalid-message  message "GAME-ADT" )))))

    dispatch))
