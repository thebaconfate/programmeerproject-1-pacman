#lang r5rs

(#%require "provided-material/Graphics.rkt" "constants.rkt" "help-procedures.rkt")
(#%provide make-draw-adt)

(define (make-draw-adt pixels-horizontal pixels-vertical)
  (let ((window (make-window pixels-horizontal pixels-vertical "Pacman")))

    ((window 'set-background!) "black")



    ;; layer for static objects like walls, coins and fruit.
    (define static-layer (window 'make-layer))
    ;; layer for dynamic objects like pacman and ghosts.
    (define dynamic-layer (window 'make-layer))

    ;; tiles
    (define tiles (make-vector 6 '()))
    (define pacman-tiles (lambda () (vector-ref tiles 0)))
    (define ghost-tiles (lambda () (vector-ref tiles 1)))
    (define coin-tiles (lambda () (vector-ref tiles 2)))
    (define fruit-tiles (lambda () (vector-ref tiles 3)))
    (define wall-tiles (lambda () (vector-ref tiles 4)))
    (define powerup-tiles (lambda () (vector-ref tiles 5)))

    (define set-pacman-tiles!
      (lambda (adt-and-sequence)
        (vector-set! tiles 0 adt-and-sequence)))

    ;; draw-object! :: any tile -> /
    (define (draw-object! object tile)
      (let* ((object-x (object 'get-x))
             (object-y (object 'get-y))
             (screen-x (* cel-width-px object-x))
             (screen-y (* cel-height-px object-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

    (define (save-tiles! adt tile-or-sequence)
      (cond
        ((pacman? adt)(set-pacman-tiles! (cons (cons adt tile-or-sequence) (pacman-tiles))))))


    (define (add-to-layer adt tile-or-sequence)
      (cond
        ((pacman? adt) (add-to-dynamic-layer tile-or-sequence))))

    (define (add-to-static-layer tile-or-sequence)
      ((static-layer 'add-drawable) tile-or-sequence))

    (define (add-to-dynamic-layer tile-or-sequence)
      ((dynamic-layer 'add-drawable) tile-or-sequence))

    (define (cons-pacman-tiles adt tile-or-sequence)
      (cons (cons adt tile-or-sequence) (pacman-tiles)))

    (define (save-and-add-to-layer adt tile-or-sequence)
      (cond
        ((pacman? adt)(set-pacman-tiles! (cons-pacman-tiles adt tile-or-sequence))
                      (add-to-dynamic-layer tile-or-sequence))))

    (define (add-object! object-adt png-mask-pairs)
      (let ((tiles '()))
        (let construct-tiles ((pairs png-mask-pairs))
          (if (not (null? pairs))
              (let* ((current-pair (car pairs))
                     (png (car current-pair))
                     (mask (cdr current-pair))
                     (tile (make-bitmap-tile png mask)))
                (set! tiles (cons tile tiles))
                (construct-tiles (cdr pairs)))))
        (if (> (length tiles) 1)
            (let ((tile-sequence (make-tile-sequence tiles)))
              (save-and-add-to-layer object-adt tile-sequence)
              tile-sequence)
            (let ((tile (car tiles)))
              (save-and-add-to-layer object-adt tiles)
              tile))))

    (define (get-object object-adt object-tiles pngs-masks-pairs)
      (let ((result (assoc object-adt object-tiles)))
        (if result
            (cdr result)
            (add-object! object-adt pngs-masks-pairs))))

    (define (draw-pacman! pacman-adt)
      (let ((tile-sequence (get-object pacman-adt
                                       (pacman-tiles)
                                       (list
                                        (cons "images/pacman-3.png" "images/pacman-3_mask.png")
                                        (cons "images/pacman-2.png" "images/pacman-2_mask.png")
                                        (cons "images/pacman-1.png" "images/pacman-1_mask.png")
                                        (cons "images/pacman-2.png" "images/pacman-2_mask.png")))))
        (draw-object! pacman-adt tile-sequence)))

    (define (draw-coin! coin)
      (let ((tile (get-object coin (coin-tiles) (list (cons "images/coin.png" "images/coin_mask.png")))))
        (draw-object! coin tile)))


    (define (draw-edible! edible-adt)
      (let ((edible-type (edible-adt 'type)))
        (cond
          ((eq? edible-type coin-type)(draw-coin! edible-adt)))))

    (define (set-key-procedure! proc)
      ((window 'set-key-callback!) proc))

    (define (set-game-loop-procedure! proc)
      ((window 'set-update-callback!) proc))

    (define draw-dispatch
      (lambda (message)
        (cond
          ((eq? message 'set-key-procedure!)set-key-procedure!)
          ((eq? message 'set-game-loop-procedure!)set-game-loop-procedure!)
          ((eq? message 'draw-pacman!)draw-pacman!))))
    draw-dispatch))
