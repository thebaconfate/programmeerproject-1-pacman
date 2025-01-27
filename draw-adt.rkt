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
    (define tiles (make-vector 5 '()))
    (define pacman-tiles (lambda () (vector-ref tiles 0)))
    (define pacman-sprites '(("images/pacman-3.png" . "images/pacman-3_mask.png")
                             ("images/pacman-2.png" . "images/pacman-2_mask.png")
                             ("images/pacman-1.png" . "images/pacman-1_mask.png")
                             ("images/pacman-2.png" . "images/pacman-2_mask.png")))
    (define ghost-tiles (lambda () (vector-ref tiles 1)))
    (define fruit-tiles (lambda () (vector-ref tiles 2)))
    (define wall-tiles (lambda () (vector-ref tiles 3)))
    (define powerup-tiles (lambda () (vector-ref tiles 4)))

    (define set-pacman-tiles! (lambda (adt-and-sequence) (vector-set! tiles 0 adt-and-sequence)))

    ;; draw-object! :: any tile -> /
    ;; TODO refactor this properly
    (define (draw-object! object tile)
      (let* ((object-x (object 'get-x))
             (object-y (object 'get-y))
             (screen-x (* cel-width-px object-x))
             (screen-y (* cel-height-px object-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

    (define (add-to-static-layer tile-or-sequence)
      ((static-layer 'add-drawable) tile-or-sequence))

    (define (add-to-dynamic-layer tile-or-sequence)
      ((dynamic-layer 'add-drawable) tile-or-sequence))

    (define (save-and-add-to-layer adt tile-sequence)
      (define (create-new-tiles old-value) (cons (cons adt tile-sequence) old-value))
      (case (adt 'get-type)
        ((pacman)
         (set-pacman-tiles! (create-new-tiles (pacman-tiles)))
         (add-to-dynamic-layer tile-sequence))))

    (define (construct-tiles png-mask-pairs)
      (if (null? png-mask-pairs)
          '()
          (let* ((first-pair (car png-mask-pairs))
                 (sprite (car first-pair))
                 (mask (cdr first-pair)))
            (cons (make-bitmap-tile sprite mask)
                  (construct-tiles (cdr png-mask-pairs))))))

    ;; TODO refactor this to a proper add-object function
    (define (add-object! object-adt png-mask-pairs)
      (let ((tiles (construct-tiles png-mask-pairs)))
        (let ((tile-sequence (make-tile-sequence tiles)))
          (save-and-add-to-layer object-adt tile-sequence)
          tile-sequence)))

    (define (get-object object-adt object-tiles pngs-masks-pairs)
      (let ((result (assoc object-adt object-tiles)))
        (if result
            (cdr result)
            (add-object! object-adt pngs-masks-pairs))))

    (define (get-tiles-storage-and-sprites adt)
      (case (adt 'get-type)
        ((pacman)(cons (pacman-tiles) pacman-sprites))))

    (define (draw! adt)
      (let* ((adt-drawables (get-tiles-storage-and-sprites adt))
             (tiles-storage (car adt-drawables))
             (sprites (cdr adt-drawables))
             (tile-sequence (get-object adt tiles-storage sprites)))
        (draw-object! adt tile-sequence)
        (tile-sequence 'set-next!)))

    (define (set-key-procedure! proc)
      ((window 'set-key-procedure!) proc))

    (define (set-game-loop-procedure! proc)
      ((window 'set-game-loop-procedure!) proc))

    (define draw-dispatch
      (lambda (message)
        (cond
          ((eq? message 'set-key-procedure!) set-key-procedure!)
          ((eq? message 'set-game-loop-procedure!) set-game-loop-procedure!)
          ((eq? message 'draw!) draw!))))
    draw-dispatch))
