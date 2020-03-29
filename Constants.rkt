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
(define out-of-bounds-left (- game-width-edge game-step))




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


;;
;; Start-Values
;;


; Pacman
(define start-value-x-pacman 14)
(define start-value-y-pacman 23)

;; Walls
;; OUTER-Walls
;; NOTE: We have height, from and to values. these lists are used to create coins on a horizontal level
(define special-left-pos             (cons 13 0))
(define special-right-pos            (cons 14 0))
(define special-vertical-left-left   (cons 0 25))
(define special-vertical-left-right  (cons 0 24))
(define special-vertical-right-right (cons 27 24))
(define special-vertical-right-left  (cons 27 25))

(define list-of-horizontal-outer-walls-height      (list  0  0  9  9 13 13 15 15 19 19 30))
(define list-of-horizontal-outer-walls-widths-from (list  1 15  1 23  0 23  0 23  1 23  1))
(define list-of-horizontal-outer-walls-widths-to   (list 12 26  4 26  4 27  4 27  4 26 26))
;: NOTE: We have a width, from and to values. These lists are used to create coins vertical level.
(define list-of-vertical-outer-walls-widths        (list  0    0  5  5 22 22 27 27))
(define list-of-vertical-outer-walls-heights-from  (list  1   20 10 16 10 16  1 20))
(define list-of-vertical-outer-walls-heights-to    (list  8   29 12 18 12 18  8 29))
;; OUTER-Corners 
(define list-of-left-top-outer-corner-widths            (list  0  0 22 22))
(define list-of-left-top-outer-corner-heights-from      (list  0 19  9 15))
(define list-of-left-bottom-outer-corner-widths         (list  0  0 22 22))
(define list-of-left-bottom-outer-corner-heights-from   (list  9 30 13 19))
(define list-of-right-top-outer-corners-widths          (list  5  5 27 27))
(define list-of-right-top-outer-corners-heights-from    (list  9 15  0 19))
(define list-of-right-bottom-outer-corners-widths       (list  5  5 27 27))
(define list-of-right-bottom-outer-corners-heights-from (list 13 19  9 30))
;; INNER-corners-small
(define list-of-right-top-inner-corner-small-widths          (list  8  8 20))
(define list-of-right-top-inner-corner-small-heights-from    (list  9 27 27))
(define list-of-right-bottom-inner-corner-small-widths       (list  8 14 14 14 23))
(define list-of-right-bottom-inner-corner-small-heights-from (list 10  7 19 25 22))
(define list-of-left-top-inner-corner-small-widths           (list  7 19 19))
(define list-of-left-top-inner-corner-small-heights-from     (list 27  9 27))
(define list-of-left-bottom-inner-corner-small-widths        (list  4 13 13 13 19))
(define list-of-left-bottom-inner-corner-small-heights-from  (list 22  7 19 25 10))
;; Inner-corners-big
(define list-of-left-top-inner-corner-big-width            (list  2  2  2  2  7  7  7  7  7 10 10 10 16 16 16 16 19 19 19 22 22 22 25))
(define list-of-left-top-inner-corner-big-heights-from     (list  2  6 21 27  2  6 15 21 24  6 18 24  2  9 21 27  6 15 24  2  6 21 24))
(define list-of-right-top-inner-corner-big-width           (list  2  5  5  5  8  8  8 11 11 11 11 17 17 17 20 20 20 20 20 25 25 25 25))
(define list-of-right-top-inner-corner-big-heights-from    (list 24  2  6 21  6 15 24  2  9 21 27  6 18 24  2  6 15 21 24  2  6 21 27))
(define list-of-left-bottom-inner-corner-big-width         (list  2  2  2  2  4  7  7  7  7 10 10 10 13 13 13 13 16 16 16 16 16 19 19 22 22 22 25))
(define list-of-left-bottom-inner-corner-big-heights-from  (list  4  7 22 28 25  4 13 19 22  7 19 25  4 10 22 28  4 10 22 22 28 13 19  4  7 25 25))
(define list-of-right-bottom-inner-corner-big-width        (list  2  5  5  5  8  8 11 11 11 11 14 14 14 14 17 17 17 20 20 20 20 23 25 25 25 25))
(define list-of-right-bottom-inner-corner-big-heights-from (list 25  4  7 25 13 19  4 10 22 28  4 10 22 28  7 19 25  4 13 19 22 25  4  7 22 28))
;; INNER-horizontals
(define list-of-top-horizontal-walls-heights        (list 2  2  2  2  6  6  6  9  9 18 21 21 21 21 24 24 24 27 27 27 27))
(define list-of-top-horizontal-walls-widths-from    (list 3  8 17 23  3 11 23  9 17 11  3  8 17 23  1 11 26  3  9 17 21))
(define list-of-top-horizontal-walls-widths-to      (list 4 10 19 24  4 16 24 10 18 16  4 10 19 24  1 16 26  6 10 18 24))
(define list-of-bottom-horizontal-walls-heights     (list 4  4  4  4  7  7  7  7 10 10 19 19 22 22 22 22 25 25 25 25 28 28))
(define list-of-bottom-horizontal-walls-widths-from (list 3  8 17 23  3 11 15 23  9 17 11 15  3  8 17 24  1 11 15 26  3 17))
(define list-of-bottom-horizontal-walls-widths-to   (list 4 10 19 24  4 12 16 24 10 18 12 16  3 10 19 24  1 12 16 26 10 24))
;; Inner-Verticals
(define list-of-left-vertical-walls-widths          (list 2  4  7  7  7  7 13 13 13 13 16 19 19 19 19 22 22))
(define list-of-left-vertical-walls-heights-from    (list 3 23  3  7 16 25  1  8 20 26  3  7 11 16 25  3 22))
(define list-of-left-vertical-walls-heights-to      (list 3 24  3 12 18 26  3  9 21 27  3  8 12 18 26  3 24))
(define list-of-right-vertical-walls-widths         (list 5  5  8  8  8  8 11 14 14 14 14 20 20 20 20 23 25))
(define list-of-right-vertical-walls-heights-from   (list 3 22  7 11 16 25  3  1  8 20 26  3  7 16 25 23  3))
(define list-of-right-vertical-walls-heights-to     (list 3 25  8 12 18 26  3  3  9 21 27  3 12 18 26 24  3)) 

;; Coins
;; NOTE: We have height, from and to values. these lists are used to create coins on a horizontal level
(define list-of-coins-heights      (list  1  1  5 8  8  8  8 20 20 23 23 23 26 26 26 26 29))
(define list-of-coins-widths-from  (list  1 15  1 1  9 15 21  1 15  2  6 24  1  9 15 21  1))
(define list-of-coins-widths-to    (list 12 26 26 6 12 18 26 12 26  3 21 25  6 12 18 26 26))
;: NOTE: We have a width, from and to values. These lists are used to create coins vertical level.
(define list-of-coins-widths       (list 1 1  1  1  3  6 9  9 12 12 12 15 15 15 18 18 21 24 26 26 26 26))
(define list-of-coins-heights-from (list 1 4 20 26 23  1 5 23  1 20 26  1 20 26  5 23  1 23  1  4 20 26))
(define list-of-coins-heights-to   (list 2 8 22 29 26 26 8 26  5 23 29  5 23 29  8 26 26 26  2  8 22 29))




;;variable used is in miliseconds
;(define XXXXXX-refresh-rate 20000)

;;speed at wich pacman moves in miliseconds ;beginvalue 300
(define pacman-speed 1000)


;;this way there isn't a problem with collision
;;(so collision doesn't have to be on the same pixel)
(define cel-width-px (/ window-width-px true-game-width))
(define cel-height-px (/ window-height-px true-game-height))

;;entity-values
(define free-spot 0)



