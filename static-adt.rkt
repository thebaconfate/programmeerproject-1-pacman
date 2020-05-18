;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Static ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-static-adt static-position type)

    ;;
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;
    
    (define (draw! draw-adt)
      ((draw-adt 'draw-static!) dispatch-static))
    

    (define (dispatch-static msg)
      (cond
        ((eq? msg 'position)static-position)
        ((eq? msg 'type)type)
        ((eq? msg 'draw!)draw!)))
    dispatch-static)