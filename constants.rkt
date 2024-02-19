#lang r5rs

(#%provide (all-defined))

(define game-width 27)
(define beta-correction 2)
(define true-game-width (+ game-width beta-correction))

(define game-height 30)
(define alpha-correction 2)
(define true-game-height (+ game-height alpha-correction))

(define pixels²-per-position 25)
(define window-width-px (* pixels²-per-position true-game-width))
(define window-height-px (* pixels²-per-position true-game-height))

(define cel-width-px (/ window-width-px true-game-width))
(define cel-height-px (/ window-height-px true-game-height))


(define coin-type 'coin)
(define coin-score-value 1)
(define coin-locations-level-1 (list (cons 8 19)
                                     (cons 9 19)
                                     (cons 10 19)
                                     (cons 11 19)
                                     (cons 12 19)
                                     (cons 13 19)
                                     (cons 14 19)
                                     (cons 15 19)
                                     (cons 16 19)
                                     (cons 17 19)
                                     (cons 18 19)
                                     (cons 19 19)
                                     (cons 8 21)
                                     (cons 9 21)
                                     (cons 10 21)
                                     (cons 11 21)
                                     (cons 12 21)
                                     (cons 13 21)
                                     (cons 14 21)
                                     (cons 15 21)
                                     (cons 16 21)
                                     (cons 17 21)
                                     (cons 18 21)
                                     (cons 19 21)
                                     (cons 8 23)
                                     (cons 9 23)
                                     (cons 10 23)
                                     (cons 11 23)
                                     (cons 12 23)
                                     (cons 13 23)
                                     (cons 14 23)
                                     (cons 15 23)
                                     (cons 16 23)
                                     (cons 17 23)
                                     (cons 18 23)
                                     (cons 19 23)))


(define wall-type 'wall)
(define wall-locations-level-1 '((1 1)(10 10)))

;; Directions
(define up 'up)
(define down 'down)
(define left 'left)
(define right 'right)


(define pacman-type 'pacman)