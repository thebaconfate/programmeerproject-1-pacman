#lang r5rs

(#%require "constants.rkt" "grid-adt.rkt" "pacman-adt.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((static-grid (make-grid-adt height width))
         (pacman (make-pacman-adt 5 5)))

    (define (draw-pacman! draw-adt)
      ((pacman 'draw!) draw-adt))


    (define (draw! draw-adt)
      (draw-pacman! draw-adt))

    (define level-dispatch
      (lambda (message)
        (cond
          ((eq? message 'draw!)draw!))))
    level-dispatch))