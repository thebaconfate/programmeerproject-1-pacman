;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Moveable ADT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (make-moveable-adt moveable-position type)
  (let ((direction 'left)
        (colour type)
        (score 0))

    ;;
    ;; private-procedures
    ;; 
    
    
    ;;
    ;; public-procedures
    ;;


    ;; score! :: integer -> /
    (define (score! new-score)
      (set! score new-score))
   

    ;; colour! any :: /
    (define (colour! new-colour)
      (set! colour new-colour))
    
    ;; take! any :: /
    (define (type! new-type)
      (set! type new-type))
    
    ;; direction! number :: symbol -> /
    (define (direction! new-direction)
      (set! direction new-direction))
    
    ;; move! :: / -> /
    (define (move!)
      (cond
        ((eq? direction 'up)((moveable-position 'y!)(- (moveable-position 'y) game-step)))
        ((eq? direction 'down)((moveable-position 'y!)(+ (moveable-position 'y) game-step)))
        ((eq? direction 'right)((moveable-position 'x!)(+ (moveable-position 'x) game-step)))
        ((eq? direction 'left)((moveable-position 'x!)(- (moveable-position 'x) game-step)))))
    
    
    ;; teleport! :: / -> /
    ;; teleport! is only used to go from one side of the map to the other
    (define (teleport!)
      (cond
        ((eq? direction 'right)((moveable-position 'x!) game-width-edge))
        ((eq? direction 'left)((moveable-position 'x!) game-width))))
    
    
    (define (draw! draw-adt)
          ((draw-adt 'draw-moveable!) dispatch-moveable))
      
  
    (define (dispatch-moveable msg)
      (cond
        ((eq? msg 'type)type)
        ((eq? msg 'type!)type!)
        ((eq? msg 'colour)colour)
        ((eq? msg 'colour!)colour!)
        ((eq? msg 'move!)move!)
        ((eq? msg 'teleport!)teleport!)
        ((eq? msg 'score)score)
        ((eq? msg 'score!)score!)
        ((eq? msg 'position)moveable-position)
        ((eq? msg 'direction)direction)
        ((eq? msg 'direction!)direction!)
        ((eq? msg 'draw!)draw!)
        (else "dispatch-moveable didn't understand the message")))
    dispatch-moveable))