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



(define (make-pacman-adt pacman-position)
  (let ((moveable-adt (make-moveable-adt pacman-position 'pacman))
        (pacman-score-value -10))
    ;;
    ;; private-procedures
    ;; 
    ((moveable-adt 'score!)pacman-score-value)
    
    ;;
    ;; public-procedures
    ;;
      
    (define (dispatch-pacman msg)
      (cond
        (else (moveable-adt msg))))
    dispatch-pacman))