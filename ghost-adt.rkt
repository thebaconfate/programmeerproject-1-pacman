;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Moveable-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Pacman ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (make-ghost-adt ghost-position)
  (let ((moveable-adt (make-moveable-adt ghost-position 'ghost)))

    ;;
    ;; private-procedures
    ;; 

    
    ;;
    ;; public-procedures
    ;;

     ;; make-red-ghost! / :: /
    (define (make-red-ghost!)
      ((moveable-adt 'type!) 'red-ghost)
      ((moveable-adt 'score!) red-ghost-value)
      ((moveable-adt 'colour!) 'red))
    

    ;; make-blue-ghost! / :: /
    (define (make-blue-ghost!)
      ((moveable-adt 'type!) 'blue-ghost)
      ((moveable-adt 'score!) blue-ghost-value)
      ((moveable-adt 'colour!) 'blue))

    ;; make-orange-ghost! / :: /
    (define (make-orange-ghost!)
      ((moveable-adt 'type!) 'orange-ghost)
      ((moveable-adt 'score!) orange-ghost-value)
      ((moveable-adt 'colour!) 'orange))

    ;; make-pink-ghost! / :: /
    (define (make-pink-ghost!)
      ((moveable-adt 'type!) 'pink-ghost)
      ((moveable-adt 'score!) pink-ghost-value)
      ((moveable-adt 'colour!) 'pink))

    (define (make-scared-ghost!)
      ((moveable-adt 'type!) 'scared-ghost))
      
    (define (dispatch-ghost msg)
      (cond
        ((eq? msg 'make-red-ghost!)make-red-ghost!)
        ((eq? msg 'make-blue-ghost!)make-blue-ghost!)
        ((eq? msg 'make-orange-ghost!)make-orange-ghost!)
        ((eq? msg 'make-pink-ghost!)make-pink-ghost!)
        ((eq? msg 'make-scared-ghost!)make-scared-ghost!)
        (else (moveable-adt msg))))
    dispatch-ghost))
