#lang r5rs

(#%require "provided-material/Graphics.rkt")
(#%require "constants.rkt")
(#%provide make-draw-adt)

(define (make-draw-adt pixels-horizontal pixels-vertical)
  (let ((window (make-window pixels-horizontal pixels-vertical "Pacman")))

    ((window 'set-background!) "black")



    ;; layer for static objects like walls, coins and fruit.
    (define static-layer (window 'make-layer))
    ;; layer for dynamic objects like pacman and ghosts.
    (define dynamic-layer (window 'make-layer))

    ;; tiles
    (define tiles (make-vector 5 '()))
    (define pacman-tiles (vector-ref tiles 0))
    (define ghost-tiles (vector-ref tiles 1))
    (define fruit-tiles (vector-ref tiles 2))
    (define wall-tiles (vector-ref tiles 3))
    (define powerup-tiles (vector-ref tiles 4))

    ;; draw-object! :: any tile -> /
    ;; TODO refactor this properly
    (define (draw-object! object tile)
      (let* ((object-x ((object 'position)'x))
             (object-y ((object 'position)'y))
             (screen-x (* cel-width-px object-x))
             (screen-y (* cel-height-px object-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))


    ;; TODO refactor this to a proper add-object function
    (define (add-object! object-adt pngs masks)
      (let ((tiles '()))
        (let construct-tiles ((rem-pngs pngs)
                              (rem-masks masks))
          (if (and (not (null? rem-pngs))
                   (not (null? rem-masks)))
              (begin (set! tiles (make-bitmap-tile (car rem-pngs)(car rem-masks)))
                     (construct-tiles (cdr rem-pngs)(cdr rem-masks)))))
        (let ((tile-sequence (make-tile-sequence tiles)))
          (set! tiles tile-sequence)
          tiles)))




    (define (set-key-procedure! proc)
      ((window 'set-key-procedure!) proc))

    (define (set-game-loop-procedure! proc)
      ((window 'set-game-loop-procedure!) proc))

    (define draw-dispatch
      (lambda (message)
        (cond
          ((eq? message 'set-key-procedure!)set-key-procedure!)
          ((eq? message 'set-game-loop-procedure!)set-game-loop-procedure!))))
    draw-dispatch))
