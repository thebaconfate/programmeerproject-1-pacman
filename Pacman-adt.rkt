;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Pacman ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (make-pacman-adt pacman-position)
  (let ((direction 'up))

    ;;
    ;; private-procedures
    ;; 
    
    
    ;;
    ;; public-procedures
    ;;
    
    
    ;; direction! number :: symbol -> /
    (define (direction! new-direction)
      (set! direction new-direction))
    
    ;; move! :: / -> /
    (define (move!)
      (cond
        ((eq? direction 'up)((pacman-position 'y!)(- (pacman-position 'y)1)))
        ((eq? direction 'down)((pacman-position 'y!)(+ (pacman-position 'y)1)))
        ((eq? direction 'right)((pacman-position 'x!)(+ (pacman-position 'x)1)))
        ((eq? direction 'left)((pacman-position 'x!)(- (pacman-position 'x)1)))))
    
    
    ;; teleport! :: / -> /
    ;; teleport! is only used to go from one side of the map to the other
    (define (teleport!)
      (cond
        ((eq? direction 'right)((pacman-position 'x!) game-width))
        ((eq? direction 'left)((pacman-position 'x!) game-width-edge))))
    
    
    (define (draw! draw-adt)
      ((draw-adt 'draw-pacman!) dispatch-pacman))

  ;  (define (kill! draw-adt)
    ;  (draw-adt 'dedraw-pacman!) dispatch-pacman)
  
    (define (dispatch-pacman msg)
      (cond
        ((eq? msg 'move!)(move!))
        ((eq? msg 'teleport!)(teleport!))
        ((eq? msg 'position)pacman-position)
        ((eq? msg 'direction)direction)
        ((eq? msg 'direction!)direction!)
        ((eq? msg 'draw!)draw!)
        ;((eq? msg 'kill!)kill!)
        ))
    dispatch-pacman))

    
    