#lang r5rs

(#%require "constants.rkt" "help-procedures.rkt" "grid-adt.rkt" "moveable-adt.rkt" "edible-adt.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((grid (make-grid-adt height width))
         (redraw-positions '()))

    (define (init-level)
      (define (get-level-positions level-and-positions-list)
        (if (or (null? (cdr level-and-positions-list))
                (= (caar level-and-positions-list) level))
            (cdar level-and-positions-list)
            (get-level-positions (cdr level-and-positions-list))))
      (let ((coin-positions (get-level-positions coins-positions-per-level)))
        (for-each (lambda (position)
                    (let* ((x (car position))
                           (y (cdr position))
                           (edible (make-edible-adt x y coin-type coin-score-value)))
                      ((grid 'write-grid!) x y edible)))
                  coin-positions)
        (set! redraw-positions (cons coin-positions redraw-positions))))

    (define (draw! draw-adt)
      (if (not (null? redraw-positions))
          (begin
            (for-each (lambda (position)
                        (let* ((x (caar position))
                               (y (cdar position)))
                          ((((grid 'read-grid) x y) 'draw!) draw-adt)))
                      redraw-positions)
            ;;(set! redraw-positions '())
            )))

    (init-level)
    (define level-dispatch
      (lambda (message)
        (cond
          ((eq? message 'draw!) draw!)
          (else (display-invalid-message message "LEVEL-ADT")))))
    level-dispatch))
