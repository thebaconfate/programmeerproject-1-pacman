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
(define pacman-type 'pacman)
(define wall-type 'wall)
(define fruit-type '())


(define coin-score-value 10)

(define coins-positions-per-level '((1 . ((1 . 1)
                                          (2 . 2)))
                                    (2 . ((1 . 1)))))

