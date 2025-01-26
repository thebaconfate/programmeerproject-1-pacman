(#%require (only racket random))
(#%require "Graphics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Globale Values en Procedures                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "constants.rkt")
(load "help-procedures.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ADT's                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "position-adt.rkt")
(load "draw-adt.rkt")
(load "grid-adt.rkt")
(load "static-adt.rkt")
(load "edible-adt.rkt")
(load "moveable-adt.rkt")
(load "wall-adt.rkt")
(load "gate-adt.rkt")
(load "coin-adt.rkt")
(load "pacman-adt.rkt")
(load "fruit-adt.rkt")
(load "ghost-adt.rkt")
(load "powerup-adt.rkt")
(load "level-adt.rkt")
(load "scoreboard-adt.rkt")
(load "game-adt.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Spel Opstarten                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define game (make-game-adt))
((game 'start))


