;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(#%require "Graphics.rkt")
;(load "Constants.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Teken ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-draw-adt pixels-horizontal pixels-vertical)
  (let ((window (make-window pixels-horizontal pixels-vertical "Pacman")))
    
    ;;
    ;; configuration of the game window
    ;;
    
    
    ((window 'set-background!) "black")       
    
    
    ;;
    ;; management of the wall tiles
    ;;
    
    (define wall-layer (window 'make-layer))
    (define wall-tiles '())
    ;; make defines of the different bitmaps of the walls



    ;;
    ;; management of the coin tiles
    ;;
    
    (define pickup-ables-layer (window 'make-layer))
    (define coin-tiles '())

    
    ;;
    ;; management of the pacman tile
    ;;
    
    (define pacman-layer (window 'make-layer))
    (define pacman-tile (make-bitmap-tile "images/pacman-1.png"
                                          "images/pacman-1_mask.png"))
    ((pacman-layer 'add-drawable) pacman-tile)
    
    
    
    
    ;;
    ;;Draw functions
    ;;
    
    ;; draw-object! :: any tile -> /
    (define (draw-object! object tile)
      (let* ((object-x ((object 'position)'x))
             (object-y ((object 'position)'y))
             (screen-x (* cel-width-px object-x))
             (screen-y (* cel-height-px object-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))
    
  

    ;; make a generic remove object function? Generic remove-object function

    
    (define (add-object! object object-adt png mask)
      (let ((new-tile
             (make-bitmap-tile png mask)))
        (cond
          ((eq? object 'coin)
           (set! coin-tiles (cons (cons object-adt new-tile) coin-tiles))
           ((pickup-ables-layer 'add-drawable) new-tile)
           new-tile)
          ((eq? object 'wall)
           (set! wall-tiles (cons (cons object-adt new-tile) wall-tiles))
           ((wall-layer 'add-drawable) new-tile)
           new-tile))))
          
    
    ;; neem-slang-stuk :: slang-stuk -> tile
    (define (get-object object object-adt object-tiles png mask)
      (let ((result (assoc object-adt object-tiles)))
        (if result
            (cdr result)
            (add-object! object object-adt png mask))))
    
    
    
    ;;
    ;;Walls
    ;;draw-walls! :: walls -> /
    (define (draw-wall! wall-adt)
      (let ((tile '()))
        (cond
          ((eq? (wall-adt 'type?) 'horizontal-outer-wall)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-h1.png" "images/wall-h1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'vertical-outer-wall)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-v1.png" "images/wall-v1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'top-left-outer-corner)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-c1.png" "images/wall-c1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'top-right-outer-corner)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-c2.png" "images/wall-c2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'bottom-left-outer-corner)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-c3.png" "images/wall-c3_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'bottom-right-outer-corner)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-c4.png" "images/wall-c4_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'top-wall-horizontal)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-nh1.png" "images/wall-nh1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'bottom-wall-horizontal)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-nh2.png" "images/wall-nh2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'left-wall-vertical)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-nv1.png" "images/wall-nv1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'right-wall-vertical)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-nv2.png" "images/wall-nv2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'right-bottom-inner-corner-small)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ic1.png" "images/wall-ic1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'right-top-inner-corner-small)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ic3.png" "images/wall-ic3_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'left-top-inner-corner-small)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ic4.png" "images/wall-ic4_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'left-bottom-inner-corner-small)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ic2.png" "images/wall-ic2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'left-top-inner-corner-big)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-oc1.png" "images/wall-oc1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'right-top-inner-corner-big)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-oc2.png" "images/wall-oc2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'left-bottom-inner-corner-big)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-oc3.png" "images/wall-oc3_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'right-bottom-inner-corner-big)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-oc4.png" "images/wall-oc4_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-right)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ih2.png" "images/wall-ih2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-left)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-ih1.png" "images/wall-ih1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-vertical-left-right)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-iv1.png" "images/wall-iv1_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-vertical-left-left)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-iv2.png" "images/wall-iv2_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-vertical-right-right)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-iv3.png" "images/wall-iv3_mask.png"))
           (draw-object! wall-adt tile))
          ((eq? (wall-adt 'type?) 'special-vertical-right-left)
           (set! tile (get-object 'wall wall-adt wall-tiles "images/wall-iv4.png" "images/wall-iv4_mask.png"))
           (draw-object! wall-adt tile))
          

          

          
          ;;insert more wall images as you create them
        )))
    
    
    ;;NOTE: zorg dat de drawfunctie van walls verschillende walls kan tekenen.
    
    
    
    
    ;;
    ;;Pacman
    ;;draw-Pacman! :: Pacman -> /
    (define (draw-pacman! pacman-adt)
      (draw-object! pacman-adt pacman-tile))
    
    ;;dedraw-pacman! :: pacman -> /

    
    ;;
    ;;Coin
    ;;draw-coin! :: coin -> /
    (define (draw-coin! coin-adt)
      (let ((tile (get-object 'coin coin-adt coin-tiles "images/coin.png" "images/coin_mask.png")))
        (draw-object! coin-adt tile)))
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;
    ;;Call-backs
    ;;
    
    ;;
    ;; set-game-function
    (define (set-game-loop-procedure! fun)
      ((window 'set-update-callback!) fun))
    
    ;;
    ;; set-key-function
    (define (set-key-procedure! fun)
      ((window 'set-key-callback!) fun))
    
    
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-draw-adt msg)
      (cond
        ((eq? msg 'set-key-procedure!)set-key-procedure!)
        ((eq? msg 'set-game-loop-procedure!)set-game-loop-procedure!)
        ((eq? msg 'draw-pacman!)draw-pacman!)
        ((eq? msg 'draw-coin!)draw-coin!)
        ((eq? msg 'draw-wall!)draw-wall!)
        ((eq? msg 'dedraw-pacman!)dedraw-pacman!)))
        
        dispatch-draw-adt))