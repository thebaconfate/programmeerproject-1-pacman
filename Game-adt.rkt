;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(#%require "Graphics.rkt")
;(load "Level-adt.rkt")
;(load "Draw-adt.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Game ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-game-adt :: / -> game
(define (make-game-adt)
  (let ((level-adt (make-level-adt true-game-width true-game-height))
        (draw-adt (make-draw-adt window-width-px window-height-px)))
    
    ;; key-procedure :: symbol, any -> /
    (define (key-procedure status key)
      ;; status is equal to:
      ;; 'pressed: when the key is pressed
      ;; 'released: when the key is released
      ;; 'when pressed for a longer duration then the procedure is recalled multiple times for the same key

      (if (eq? status 'pressed)
        ((level-adt 'key!) key)))
    
    ;; game-loop-procedure :: number -> /
    (define (game-loop-procedure delta-tijd)
      ((level-adt 'update!) delta-tijd)
      ((level-adt 'draw!) draw-adt))
    
    ;; procedure to start the game
    ;; start :: / -> /
    (define (start)
      ;; sets the callbacks through the draw-adt
      ((draw-adt 'set-game-loop-procedure!) game-loop-procedure)
      ((draw-adt 'set-key-procedure!) key-procedure))
    
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-game msg)
      (cond ((eq? msg 'start) start)))
    dispatch-game))
