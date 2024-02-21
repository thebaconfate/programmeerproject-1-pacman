#lang r5rs

(#%require "constants.rkt" "draw-adt.rkt" "level-adt.rkt" "static-adt.rkt" "edible-adt.rkt")
(#%provide make-game-adt)

(define (make-game-adt)
  (let* ((level 1)
         (level-adt (make-level-adt true-game-width true-game-height 1))
         (draw-adt (make-draw-adt window-width-px window-height-px)))


    (define (key-procedure status key)
      ;; status is equal to:
      ;; 'pressed: when the key is pressed
      ;; 'released: when the key is released
      ;; 'when pressed for a longer duration then the procedure is recalled multiple times for the same key

      (if (eq? status 'pressed)
          ((level-adt 'key!) key)))

    (define (game-loop-procedure delta-tijd)
      (begin
        ((level-adt 'update!) delta-tijd)
        ((level-adt 'draw!) draw-adt)))


    (define (start)
      (define make-wall-adt
        (lambda (x y)
          (make-static-adt x y wall-type)))
      (define make-coin-adt
        (lambda (x y)
          (make-edible-adt x y coin-score-value coin-type)))
      ((draw-adt 'set-game-loop-procedure!) game-loop-procedure)
      ((draw-adt 'set-key-procedure!) key-procedure)
      ((level-adt 'spawn-items!) make-coin-adt coin-locations-level-1)
      ((level-adt 'spawn-items!) make-wall-adt wall-locations-level-1)
      ((level-adt 'draw-all!) draw-adt))
    ;; sets the callbacks through the draw-adt

    (define dispatch
      (lambda (message)
        (cond
          ((eq? message 'start)(start))
          (else "Unknown message"))))
    dispatch))