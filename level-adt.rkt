#lang r5rs

(#%require "constants.rkt" "help-procedures.rkt" "grid-adt.rkt" "moveable-adt.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((static-grid (make-grid-adt height width))
         (pacman (make-moveable-adt 5 5 'pacman)))

    (define (draw-pacman! draw-adt)
      ((pacman 'draw!) draw-adt))

    (define (draw! draw-adt)
      (begin
        (draw-pacman! draw-adt)))

    (define level-dispatch
      (lambda (message)
        (cond
          ((eq? message 'draw!) draw!)
          (else (display-invalid-message message "LEVEL-ADT")))))
    level-dispatch))
