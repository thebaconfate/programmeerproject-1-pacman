#lang r5rs

(#%require "constants.rkt" "grid-adt.rkt" "pacman-adt.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((static-grid (make-grid-adt height width))
         (pacman (make-pacman-adt 5 5)))


    (define (spawn-items! make-proc list-of-locations)
      (let loop ((l list-of-locations))
        (if (not (null? l))
            (let* ((pospair (car l))
                   (x (car pospair))
                   (y (cadr pospair))
                   (item (make-proc x y)))
              ((static-grid 'write-grid!) y x item)
              (loop (cdr l)))))
      (static-grid 'to-string))


    (define (draw-pacman! draw-adt)
      ((pacman 'draw!) draw-adt))

    (define (move-pacman! direction)
      ((pacman 'move-with-direction!) direction))

    (define (draw-all! draw-adt)
      (define item? (lambda (item) (procedure? item)))
      (define draw-if-item
        (lambda (item)
          (if (item? item)
              ((item 'draw!) draw-adt))))
      ((static-grid 'for-each-grid) draw-if-item)
      (draw-pacman! draw-adt))


    (define (direction? value)
      (or (eq? value 'up)
          (eq? value 'down)
          (eq? value 'left)
          (eq? value 'right)))

    ;; key! :: any -> /
    (define (key! key)
      (cond
        ((direction? key )(move-pacman! key))
        (else (display key))))

    (define (draw! draw-adt)
      (draw-pacman! draw-adt))

    (define (update! delta-time)
      (display "updateing!"))

    (define level-dispatch
      (lambda (message)
        (cond
          ((eq? message 'spawn-items!) spawn-items!)
          ((eq? message 'update!) update!)
          ((eq? message 'draw!) draw!)
          ((eq? message 'draw-all!) draw-all!)
          ((eq? message 'key!) key!))))
    level-dispatch))