#lang r5rs

(#%require "constants.rkt" "grid-adt.rkt" "pacman-adt.rkt" "help-procedures.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((static-grid (make-grid-adt height width))
         (pacman (make-pacman-adt (car pacman-location)(cadr pacman-location)))
         (redraw-items '()))

    (define (write-grid! x y value)
      ((static-grid 'write-grid!) y x value))

    (define (read-grid x y)
      ((static-grid 'read-grid) y x))

    (define (spawn-items! make-proc list-of-locations)
      (for-each (lambda (pospair)
                  (let* ((x (car pospair))
                         (y (cadr pospair))
                         (item (make-proc x y)))
                    (write-grid! x y item)))
                list-of-locations))

    (define (make-empty) 0)

    (define (draw-pacman! draw-adt)
      ((pacman 'draw!) draw-adt))

    (define (eat-edible! item draw-adt)
      ((item 'remove!) draw-adt))

    (define (add-to-redraw! item)
      (set! redraw-items (cons item redraw-items)))

    (define (move-pacman! direction)
      ((pacman 'set-direction!) direction)
      (let ((nex-pos ((pacman 'next-position) direction)))
        (cond
          ((< (nex-pos 'get-x)(* width 0))((pacman 'teleport!) (- width 1)))
          ((> (nex-pos 'get-x)(- width 1))((pacman 'teleport!) 0))
          (else
           (let ((item (read-grid (nex-pos 'get-x) (nex-pos 'get-y))))
             (if (not (wall? item))
                 (begin
                   ((pacman 'set-position!) nex-pos)
                   (cond
                     ((edible? item)(add-to-redraw! item))))))))))

    (define (draw-all! draw-adt)
      (define object? (lambda (item) (procedure? item)))
      (define draw-if-object
        (lambda (item)
          (if (object? item)
              ((item 'draw!) draw-adt))))
      ((static-grid 'for-each-grid) draw-if-object)
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
      (draw-pacman! draw-adt)
      (define eat-and-remove
        (lambda (item)
          (eat-edible! item draw-adt)
          (write-grid! (item 'get-x) (item 'get-y) (make-empty))))
      (for-each eat-and-remove redraw-items)
      (set! redraw-items '()))

    (define (update! delta-time)
      "dummy update function")

    (define level-dispatch
      (lambda (message)
        (cond
          ((eq? message 'spawn-items!) spawn-items!)
          ((eq? message 'update!) update!)
          ((eq? message 'draw!) draw!)
          ((eq? message 'draw-all!) draw-all!)
          ((eq? message 'key!) key!))))
    level-dispatch))