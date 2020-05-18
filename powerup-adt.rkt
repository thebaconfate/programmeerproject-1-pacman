;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Position-adt.rkt")
(load "Edible-adt.rkt")
(load "Draw-adt.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Powerup ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (make-powerup-adt powerup-position)
  (let ((edible-adt (make-edible-adt powerup-position 'powerup)))
    ((edible-adt 'drop!))
    ;;
    ;; private-procedures
    ;;
   
        
    ;;
    ;; public-procedures
    ;;
     

    ;;
    ;; Dispatch
    ;;

    (define (dispatch-powerup msg)
      (cond
        (else (edible-adt msg))))
    dispatch-powerup))
