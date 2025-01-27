#lang r5rs

(#%require "constants.rkt" "help-procedures.rkt" "pacman-adt.rkt" "draw-adt.rkt" "level-adt.rkt")
(#%provide make-game-adt)

(define (make-game-adt)
  (let* ((level 1)
         (pacman (make-pacman-adt 5 5))
         (level-adt (make-level-adt true-game-width true-game-height level))
         (draw-adt (make-draw-adt window-width-px window-height-px))
         (able-to-move #f)
         (time 0))

    (define (key-procedure status key)
      (begin
        (if (eq? 'pressed status)
            (begin
              ((pacman 'set-direction!) key)
              (if able-to-move
                  (begin
                    ((pacman 'move!))
                    (set! able-to-move #f)))))))

    (define (game-loop-procedure delta-time)
      (begin
        (set! time (+ time delta-time))
        (if (> time 200)
            (begin
              (set! able-to-move #t)
              ((pacman 'draw!) draw-adt)
              ;;   ((level-adt 'update!) draw-adt)
              ;;  ((level-adt 'draw!) draw-adt)
              (set! time 0)))))

    (define (start)
      ((draw-adt 'set-game-loop-procedure!) game-loop-procedure)
      ((draw-adt 'set-key-procedure!) key-procedure))

    (define dispatch
      (lambda (message)
        (cond
          ((eq? message 'start) start)
          (else (display-invalid-message  message "GAME-ADT" )))))

    dispatch))
