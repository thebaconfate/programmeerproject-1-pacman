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

    (define (make-get-tiles-proc index)
      (lambda () (vector-ref tiles index)))

    (define pacman-tiles (make-get-tiles-proc 0))
    (define ghost-tiles (make-get-tiles-proc 1))
    (define coin-tiles (make-get-tiles-proc 2))
    (define fruit-tiles (make-get-tiles-proc 3))
    (define wall-tiles (make-get-tiles-proc 4))
    (define powerup-tiles (make-get-tiles-proc 5))


    (define (make-set-tiles-proc index)
      (lambda (adt-and-sequence)
        (vector-set! tiles index adt-and-sequence)))

    (define set-pacman-tiles! (make-set-tiles-proc 0))
    (define set-coin-tiles! (make-set-tiles-proc 2))
    (define set-wall-tiles! (make-set-tiles-proc 4))

    ;; draw-object! :: any tile -> /
    (define (draw-object! object tile)
      (let* ((object-x (object 'get-x))
             (object-y (object 'get-y))
             (screen-x (* cel-width-px object-x))
             (screen-y (* cel-height-px object-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

    (define (make-double-cons-tiles get-tiles-proc)
      (lambda (adt tile-or-sequence)
        (cons (cons adt tile-or-sequence) (get-tiles-proc))))

    (define (save-tiles! adt tile-or-sequence)
      (cond
        ((pacman? adt)(set-pacman-tiles! (cons (cons adt tile-or-sequence) (pacman-tiles))))))


    (define (add-to-layer adt tile-or-sequence)
      (cond
        ((pacman? adt) (add-to-dynamic-layer tile-or-sequence))
        ((wall? adt) (add-to-static-layer tile-or-sequence))
        ((coin? adt) (add-to-static-layer tile-or-sequence))))

    (define (add-to-static-layer tile-or-sequence)
      ((static-layer 'add-drawable) tile-or-sequence))

    (define (add-to-dynamic-layer tile-or-sequence)
      ((dynamic-layer 'add-drawable) tile-or-sequence))

    (define cons-pacman-tiles (make-double-cons-tiles pacman-tiles))
    (define cons-coin-tiles (make-double-cons-tiles coin-tiles))
    (define cons-wall-tiles (make-double-cons-tiles wall-tiles))

    (define (save-and-add-to-layer adt tile-or-sequence)
      (cond
        ((pacman? adt)
         (set-pacman-tiles! (cons-pacman-tiles adt tile-or-sequence))
         (add-to-dynamic-layer tile-or-sequence))
        ((wall? adt)
         (set-wall-tiles! (cons-wall-tiles adt tile-or-sequence))
         (add-to-static-layer tile-or-sequence))
        ((coin? adt)
         (set-coin-tiles! (cons-coin-tiles adt tile-or-sequence))
         (add-to-static-layer tile-or-sequence))))

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
              (save-and-add-to-layer object-adt tile)
              tile))))

    (define (get-object object-adt object-tiles pngs-masks-pairs)
      (let ((result (assoc object-adt object-tiles)))
        (if result
            (cdr result)
            (add-object! object-adt pngs-masks-pairs))))


    (define (make-draw-function get-tiles-proc list-of-pngs-masks-pairs)
      (lambda (adt)
        (let ((tile-sequence (get-object adt (get-tiles-proc) list-of-pngs-masks-pairs)))
          (draw-object! adt tile-sequence))))

    (define (make-static-dedraw-function get-tiles-proc set-tiles-proc!)
      (lambda (adt)
        (let* ((result (assoc adt (get-tiles-proc)))
               (tile-or-sequence (cdr result)))
          ((static-layer 'remove-drawable) tile-or-sequence)
          (set-tiles-proc! (remove adt (get-tiles-proc))))))

    (define draw-pacman!
      (make-draw-function pacman-tiles
                          (list
                           (cons "images/pacman-3.png" "images/pacman-3_mask.png")
                           (cons "images/pacman-2.png" "images/pacman-2_mask.png")
                           (cons "images/pacman-1.png" "images/pacman-1_mask.png")
                           (cons "images/pacman-2.png" "images/pacman-2_mask.png"))))

    (define draw-coin!
      (make-draw-function coin-tiles
                          (list
                           (cons "images/coin.png" "images/coin_mask.png"))))

    (define remove-coin!
      (make-static-dedraw-function coin-tiles
                                   set-coin-tiles!))

    (define draw-wall!
      (make-draw-function wall-tiles
                          (list
                           (cons "images/wall-segment.png" "images/wall-segment_mask.png"))))

    (define (draw-edible! edible-adt)
      (cond
        ((coin? edible-adt)(draw-coin! edible-adt))))

    (define (remove-edible! edible-adt)
      (cond
        ((coin? edible-adt)(remove-coin! edible-adt))))

    (define (draw-static! static-adt)
      (let ((static-type (static-adt 'get-type)))
        (cond
          ((eq? static-type wall-type)(draw-wall! static-adt)))))

    (define (set-key-procedure! proc)
      ((window 'set-key-callback!) proc))

    (define (set-game-loop-procedure! proc)
      ((window 'set-update-callback!) proc))

    (define draw-dispatch
      (lambda (message)
        (cond
          ((eq? message 'set-key-procedure!) set-key-procedure!)
          ((eq? message 'set-game-loop-procedure!) set-game-loop-procedure!)
          ((eq? message 'draw-pacman!) draw-pacman!)
          ((eq? message 'draw-static!) draw-static!)
          ((eq? message 'draw-edible!) draw-edible!)
          ((eq? message 'remove-edible!) remove-edible!))))
    draw-dispatch))
