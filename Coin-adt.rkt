;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(#%require "Graphics.rkt")
;(load "Constants.rkt")
;(load "Position-adt.rkt")
;(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Coin ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-coin-adt coin-position)
  (let ((on-the-ground? #t))


    
    ;;
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;
    
    (define (pick-up!)
      (set! on-the-ground? #f))

    (define (drop!)
      (set! on-the-ground? #t))

    (define (draw! draw-adt)
          ((draw-adt 'draw-coin!) dispatch-coin))
    

    (define (dispatch-coin msg)
      (cond
        ((eq? msg 'position)coin-position)
        ((eq? msg 'on-the-ground?)on-the-ground?)
        ((eq? msg 'pick-up!)(pick-up!))
        ((eq? msg 'drop!)(drop!))
        ((eq? msg 'draw!)draw!)))
    dispatch-coin))


    