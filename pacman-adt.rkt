#lang r5rs

(#%require "moveable-adt.rkt")
(#%provide make-pacman-adt)


(define (make-pacman-adt x y)
  (let ((moveable (make-moveable-adt x y 'pacman))
        (rotations '(0 . #f)))

    (define enumerated-directions '((left . 0)(up . 1)(right . 2)(down . 3)))

    (define (set-direction! new-direction)
      (begin
        (if (not (eq? new-direction (moveable 'get-direction)))
            (let* ((old-idx (cdr (assoc (moveable 'get-direction) enumerated-directions)))
                   (new-idx (cdr (assoc new-direction enumerated-directions)))
                   (clockwise-rotations (modulo (- new-idx old-idx) 4))
                   (counterclockwise-rotations (modulo (- old-idx new-idx)4)))
              (if (> clockwise-rotations counterclockwise-rotations)
                  (set! rotations (cons counterclockwise-rotations 'rotate-counterclockwise))
                  (set! rotations (cons clockwise-rotations 'rotate-clockwise)))))
        ((moveable 'set-direction!) new-direction)))

    (define (reset-rotation!) (set! rotations '(0 . #f)))

    (define (draw! draw-adt)
      ((draw-adt 'draw-pacman!) pacman-dispatch))

    (define pacman-dispatch
      (lambda (message)
        (cond
          ((eq? message 'set-direction!) set-direction!)
          ((eq? message 'get-rotation) rotations)
          ((eq? message 'reset-rotation!) reset-rotation!)
          ((eq? message 'draw!) draw!)
          (else (moveable message)))))
    pacman-dispatch))
