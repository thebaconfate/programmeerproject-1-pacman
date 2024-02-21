#lang r5rs

(#%require "filereader.rkt")
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

(define level-1-file "level-1.txt")
(define level-2-file "level-2.txt")
(define level-3-file "level-3.txt")

(define (remove-dups l)
  (let loop ((l l)
             (acc '()))
    (if (null? l)
        acc
        (if (member (car l) acc)
            (loop (cdr l) acc)
            (loop (cdr l) (cons (car l) acc))))))

(define level-1 (make-filereader-adt "level-1.txt"))
(define level-2 (make-filereader-adt "level-2.txt"))
(define level-3 (make-filereader-adt "level-3.txt"))

(define coin-type 'coin)
(define coin-score-value 1)
(define coin-locations-level-1 (remove-dups (level-1 'get-coins-positions)))


(define wall-type 'wall)
(define wall-locations-level-1 (level-1 'get-wall-positions))

;; Directions
(define up 'up)
(define down 'down)
(define left 'left)
(define right 'right)


(define pacman-type 'pacman)