;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(#%require "Graphics.rkt")
;(load "Constants.rkt")
;(load "Position-adt.rkt")
;(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Wall ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-wall-adt wall-position type)

    ;;
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;
    
    (define (draw! draw-adt)
      ((draw-adt 'draw-wall!) dispatch-wall))
    

    (define (dispatch-wall msg)
      (cond
        ((eq? msg 'position)wall-position)
        ((eq? msg 'type?)type)
        ((eq? msg 'draw!)draw!)))
    dispatch-wall)