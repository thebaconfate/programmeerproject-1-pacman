;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Constants                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game boundaries in the grid (it's currently played ((27+beta)*(30+alpha))
;; NOTE: alpha is a row on the bottom of the game window that is reserved to keep pacman within the game window
;; NOTE: and be able to display other things, eg: score, lives etc.
;; NOTE: You have to adapt the game-middle-height when game-height becomes an even number
(define game-step 1)
(define game-width 27)
(define beta-correction 2)
(define true-game-width (+ game-width beta-correction))
(define game-width-edge 0)
(define game-width-right-bound (- game-width game-step))
(define game-width-left-bound (+ game-width-edge game-step))
(define out-of-bounds-right (+ game-width game-step))
(define out-of-bounds-left -1)




(define game-height 30)
(define alpha-correction 2)
(define true-game-height (+ game-height alpha-correction))
(define game-height-edge 0)
(define game-height-top-bound (+ game-height-edge game-step))
(define game-height-bottom-bound (- game-height game-step))

;; Game constants reduced to pixels for the draw-adt.
(define pixels²-per-position 25)
(define window-width-px (* pixels²-per-position true-game-width))
(define window-height-px (* pixels²-per-position true-game-height))
(define amount-of-levels 5)

;;
;; Start-Values
;;


; Pacman
(define start-value-x-pacman 14)
(define start-value-y-pacman 23)

; Ghost
(define ghost-spawn '(13 11))
(define ghost-in-box '(14 13))

;; fruit
(define fruit-x-min 7)
(define fruit-x-max 21)
(define fruit-y-min 9)
(define fruit-y-max 20)
(define fruit-generator 100)

;; Scoreboard values:
(define score-position (cons 575 275))
(define score-value-position (cons 625 300))
(define highscore-position (cons 1 275))
(define highscore-value-position (cons 75 300))
(define lives-position (cons 1 400))
(define lives-value-position (cons 75 400))

  

;; Walls
;;
;; Level 1 values:
;; Row values:
(define level-1-wall-list-of-row-y   '( 8 10 12 12 12 12 13 13 15 15 16 16 16 18 20 20 22 24))
(define level-1-wall-list-of-start-x '( 7  9  7 10 15 19  0 19  0 19  7 10 19  9  9 15  9  7))
(define level-1-wall-list-of-end-x   '(20 18  8 12 17 20  8 27  8 27  8 17 20 18 12 18 18 20))
;; Column values:
(define level-1-wall-list-of-column-x '( 7  7 10 17 20 20))
(define level-1-wall-list-of-start-y  '( 8 15 12 12  8 15))
(define level-1-wall-list-of-end-y    '(13 24 16 16 13 24))

;; Level 2 values:
;; Row values:
(define level-2-wall-list-of-row-y   '( 4  5  5  6  6  6  7  9 10 12 12 13 13 15 15 16 18 19 21 22 22 22 23 23 24))
(define level-2-wall-list-of-start-x '( 5  5 19  5 10 19 10 10 10 10 15  0 22  0 22 10 10 10 10  5 10 19  5 19  5))
(define level-2-wall-list-of-end-x   '(22  8 22  8 17 22 17 17 17 12 17  5 27  5 27 17 17 17 17  8 17 22  8 22 22))
;; Column values:
(define level-2-wall-list-of-column-x '( 5  5  7  7  8  8 10 17 19 19 20 20 22 22))
(define level-2-wall-list-of-start-y  '( 4 15  8 15  8 15 12 12  8 15  8 15  4 15))
(define level-2-wall-list-of-end-y    '(13 24 13 20 13 20 16 16 13 20 13 20 13 24))

;; Level 3 values:
;; Row values:
(define level-3-wall-list-of-row-y   '( 0  2  2  2  2  3  3  3  3  4  4  4  4  6  6  6  7  7  7  9  9  9  9 10 10 12 12 13 13 15 15 16 18 19 19 19 21 21 21 21 22 22 22 22 24 24 24 25 25 25 27 27 28 28 30))
(define level-3-wall-list-of-start-x '( 0  2  7 16 22  2  7 16 22  2  7 16 22  2 10 22  2 10 22  0  7 16 22  7 16 10 15  0 22  0 22 10 10  0 10 22  2  7 16 22  2  7 16 22  1 10 25  1 10 25  2 16  2 16  0))
(define level-3-wall-list-of-end-x   '(27  5 11 20 25  5 11 20 25  5 11 20 25  5 17 25  5 17 25  5 11 20 27 11 20 12 17  5 27  5 27 17 17  5 17 27  5 11 20 25  5 11 20 25  2 17 26  2 17 26 11 25 11 25 27))
;; Column values:
(define level-3-wall-list-of-column-x '( 0  0  4  5  5  5  7  7  7  8  8  8 10 13 13 13 13 14 14 14 14 17 19 19 19 20 20 20 22 22 22 23 27 27))
(define level-3-wall-list-of-start-y  '( 0 19 21  9 15 21  6 15 24  6 15 24 12  0  6 18 24  0  6 18 24 12  6 15 24  6 15 24  9 15 21 21  0 19))
(define level-3-wall-list-of-end-y    '( 9 30 25 13 19 25 13 19 28 13 19 28 16  4 10 22 28  4 10 22 28 16 13 19 28 13 19 28 13 19 25 25  9 30))


;; Gates
(define gate-list-of-row-y '(12))
(define gate-list-of-start-x '(13))
(define gate-list-of-end-x '(14))

;; Ones
(define one-list-of-row-y '(13 14 15))
(define one-list-of-start-x '(11 11 11))
(define one-list-of-end-x '(16 16 16))



;; Coins
;;
;; Level 1 values:
;; Row-values:
(define level-1-coin-list-of-row-y   '(19 21 23))
(define level-1-coin-list-of-start-x '( 8  8  8))
(define level-1-coin-list-of-end-x   '(19 19 19))
;;Column values:
(define level-1-coin-list-of-column-x  '( 8 19))
(define level-1-coin-list-of-start-y   '(19 19))
(define level-1-coin-list-of-end-y     '(23 23))

;; Level 2 values:
;; Row-values:
(define level-2-coin-list-of-row-y   '( 5  7  7  8 20 21 21 23))
(define level-2-coin-list-of-start-x '( 9  6 19  9  9  6 19  9))
(define level-2-coin-list-of-end-x   '(18  8 21  18 18 8 21 18))
;;Column values:
(define level-2-coin-list-of-column-x '( 6  9  9 18 18 21))
(define level-2-coin-list-of-start-y  '( 7  5 22  5 22  7))
(define level-2-coin-list-of-end-y    '(21  6 23  6 23 21))


;; Level 3 values:
;; Row-values:
(define level-3-coin-list-of-row-y   '( 1  1  5  8  8  8  8 20 20 23 23 23 26 26 26 26 29))
(define level-3-coin-list-of-start-x '( 1 15  1  1  9 15 21  1 15  2  6 24  1  9 15 21  1))
(define level-3-coin-list-of-end-x   '(12 26 26  6 12 18 26 12 26  3 21 25  6 12 18 26 26))
;;Column values:
(define level-3-coin-list-of-column-x '(1 1  1  1  3  6 9  9 12 12 12 15 15 15 18 18 21 24 26 26 26 26))
(define level-3-coin-list-of-start-y  '(1 4 20 26 23  1 5 23  1 20 26  1 20 26  5 23  1 23  1  4 20 26))
(define level-3-coin-list-of-end-y    '(2 8 22 29 26 26 8 26  5 23 29  5 23 29  8 26 26 26  2  8 22 29))



;; Powerup
;;
;; Level 1 values:
;; (X Y) values:
(define level-1-powerups '((13 20)(14 20)))

;; Level 2 values:
;; (X Y) values:
(define level-2-powerups '((9  7)(9 21)(18 7)(18 21)))


;; Level 3 values:
;; (X Y) values:
(define level-3-powerups '((1 3)(1 23)(26 3)(26 23)))



;;variable used is in miliseconds
;(define XXXXXX-refresh-rate 20000)

;;speed at wich pacman moves in miliseconds ;beginvalue 300
(define pacman-speed 600)
(define ghost-speed 550)
(define fruit-speed 40000)
(define invincibility-speed 50000)
;;25000
(define released-speed 12000)


;;this way there isn't a problem with collision
;;(so collision doesn't have to be on the same pixel)
(define cel-width-px (/ window-width-px true-game-width))
(define cel-height-px (/ window-height-px true-game-height))

;;other-entity-values
(define free-spot 0)
(define teleport-y 14)

;; Coin-value
(define coin-score 1)

;;fruit values
(define cherry-value 100)
(define strawberry-value 200)
(define orange-value 500)
(define apple-value 700)
(define melon-value 1000)

;; ghost-values
(define red-ghost-value 200)
(define blue-ghost-value (* 2 red-ghost-value))
(define orange-ghost-value (* 2 blue-ghost-value))
(define pink-ghost-value (* 2 orange-ghost-value))

;; ghost-refs
(define red-ghost-ref 0)
(define blue-ghost-ref 1)
(define orange-ghost-ref 2)
(define pink-ghost-ref 3)


;; Other values
(define one 1)
(define starting-lives 3)
(define starting-score 0)
