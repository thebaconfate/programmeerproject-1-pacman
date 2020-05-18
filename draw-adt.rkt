;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Help-procedures.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 draw ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-draw-adt pixels-horizontal pixels-vertical)
  (let ((window (make-window pixels-horizontal pixels-vertical "Pacman"))
        (half-window-width-px (half pixels-horizontal))
        (half-window-height-px (half pixels-vertical))
        (3fourths-window-height-px (3fourths pixels-vertical)))
    
    ;;
    ;; configuration of the game window
    ;;
    
    ((window 'set-background!) "black")

    ;;
    ;; management of the layers
    ;;


    ;; layer for the score
    (define score-layer (window 'make-layer))
    ;; layer for static objects like walls, coins and fruit.
    (define static-layer (window 'make-layer))
    ;; layer for pacman
    (define pacman-layer (window 'make-layer))
    ;; layer for the ghosts
    (define ghost-layer (window 'make-layer))

    
    ;;
    ;; management of the wall tiles
    ;;
    
    (define wall-tiles '())

    ;;
    ;; management of the gate tiles
    ;;
    
    (define gate-tiles '())

    ;;
    ;; management of the coin tiles
    ;;

    (define coin-tiles '())
    
    ;;
    ;; management of the pacman tile
    ;;
    
    (define pacman-tiles '())

    ;;
    ;; management of the ghost tile
    ;;
    
    (define ghost-tiles '())
    (define scared-ghost-tiles '())


    ;;
    ;; management of the fruit tiles
    ;;

    (define fruit-tiles '())

    ;;
    ;; management of the powerup tiles
    ;;
    (define powerup-tiles '())

    ;;
    ;; management of the score-tiles
    ;;

    (define score-tile (make-tile pixels-horizontal half-window-height-px))
    (define score-value-tile (make-tile pixels-horizontal half-window-height-px))
    (define highscore-tile (make-tile half-window-width-px half-window-height-px))
    (define highscore-value-tile (make-tile half-window-width-px half-window-height-px))
    (define lives-tile (make-tile half-window-width-px 3fourths-window-height-px))
    (define lives-value-tile (make-tile half-window-width-px 3fourths-window-height-px))
    
    
    
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

    ;; add-object! :: symbol adt file file -> tile
    (define (add-object! object object-adt png mask)
      (let ((new-tile
             (make-bitmap-tile png mask)))
        (cond
          ((eq? object 'pacman)
           (set! pacman-tiles (cons (cons object-adt new-tile) pacman-tiles))
           ((pacman-layer 'add-drawable) new-tile)
           new-tile)
          ((eq? object 'coin)
           (set! coin-tiles (cons (cons object-adt new-tile) coin-tiles))
           ((static-layer 'add-drawable) new-tile)
           new-tile)
          ((eq? object 'wall)
           (set! wall-tiles (cons (cons object-adt new-tile) wall-tiles))
           ((static-layer 'add-drawable) new-tile)
           new-tile)
          ((fruit? object)
           (set! fruit-tiles (cons (cons object-adt new-tile) fruit-tiles))
           ((static-layer 'add-drawable) new-tile)
           new-tile)
          ((gate? object)
           (set! gate-tiles (cons (cons object-adt new-tile) gate-tiles))
           ((static-layer 'add-drawable) new-tile)
           new-tile)
          ((powerup? object)
           (set! powerup-tiles (cons (cons object-adt new-tile) powerup-tiles))
           ((static-layer 'add-drawable) new-tile)
           new-tile)
          ((ghost? object)
           (set! ghost-tiles (cons (cons object-adt new-tile) ghost-tiles))
           ((ghost-layer 'add-drawable) new-tile)
           new-tile)
          ((scared-ghost? object)
           (set! scared-ghost-tiles (cons (cons object-adt new-tile) scared-ghost-tiles))
           ((ghost-layer 'add-drawable) new-tile)
           new-tile))))
           
          
    
    ;; get-object :: symbol adt pair file file -> /
    (define (get-object object object-adt object-tiles png mask)
      (let ((result (assoc object-adt object-tiles)))
        (if result
            (cdr result)
            (add-object! object object-adt png mask))))


    ;;clear-all :: / -> /
    (define (clear-all!)
      (dedraw-all! pacman-tiles pacman-layer)
      (set! pacman-tiles '())
      (dedraw-all! ghost-tiles ghost-layer)
      (set! ghost-tiles '())
      (dedraw-all! powerup-tiles static-layer)
      (set! powerup-tiles '())
      (dedraw-all! fruit-tiles static-layer)
      (set! fruit-tiles '())
      (dedraw-all! coin-tiles static-layer)
      (set! coin-tiles '())
      (dedraw-all! wall-tiles static-layer)
      (set! wall-tiles '())
      (dedraw-all! gate-tiles static-layer)
      (set! gate-tiles '()))

    ;;dedraw-all! :: tiles layer -> /
    (define (dedraw-all! tiles layer)
      (if (not (null? tiles))
          (let loop ((current-e (car tiles))
                       (rest-e (cdr tiles)))
              (let ((current-tile (cdr current-e)))
                (begin ((layer 'remove-drawable)current-tile)
                       (if (not (null? rest-e))
                           (loop (car rest-e) (cdr rest-e))))))))
    
                
                     


      
    
    ;;remove-object! :: layer, tile -> /
    (define (remove-object! layer tile)
      ((layer 'remove-drawable)tile))

    ;;dedraw-object :: layer tiles object-adt -> /
    (define (dedraw-object layer tiles object-adt)
      (if (null? tiles)
          "tiles are empty"
          (let loop ((base-position (object-adt 'position))
                     (current-e (car tiles))
                     (rest-e (cdr tiles)))
            (let* ((current-object (car current-e))
                   (current-tile (cdr current-e))
                   (current-position (current-object 'position)))
              (if ((base-position 'compare-position?)current-position)
                  (remove-object! layer current-tile)
                  (if (null? rest-e)
                      "object not found"
                      (loop base-position (car rest-e) (cdr rest-e))))))))
             


    ;; draw-static! :: static -> /
    (define (draw-static! static-adt)
      (cond
        ((wall? (static-adt 'type))(draw-wall! static-adt))
        ((gate? (static-adt 'type))(draw-gate! static-adt))
        (else "Not a static adt")))


    
    ;; draw-gate! :: static -> /
    (define (draw-gate! gate-adt)
      (let ((tile (get-object 'gate gate-adt gate-tiles "images/gate-segment.png" "images/gate-segment_mask.png")))          
        (draw-object! gate-adt tile)))



    
    ;;
    ;;Walls
    ;;draw-walls! :: static -> /
    (define (draw-wall! wall-adt)
      (let ((tile (get-object 'wall wall-adt wall-tiles "images/wall-segment.png" "images/wall-segment_mask.png")))          
        (draw-object! wall-adt tile)))

    
    ;;
    ;; Edibles
    ;; draw-edible! :: edible -> /
    (define (draw-edible! edible-adt)
      (cond
        ((coin? (edible-adt 'type))(draw-coin! edible-adt))
        ((powerup? (edible-adt 'type))(draw-powerup! edible-adt))
        ((fruit? (edible-adt 'type))(draw-fruit! edible-adt))
        (else "not an edible adt to draw")))

    (define (dedraw-edible! edible-adt)
      (cond
        ((coin? (edible-adt 'type))(dedraw-coin! edible-adt))
        ((powerup? (edible-adt 'type))(dedraw-powerup! edible-adt))
        ((fruit? (edible-adt 'type))(dedraw-fruit! edible-adt))
        (else "not an edible adt to dedraw")))

    

    
    ;;
    ;; Coin
    ;; draw-coin! :: coin -> /
    (define (draw-coin! coin-adt)
      (let ((tile (get-object 'coin coin-adt coin-tiles "images/coin.png" "images/coin_mask.png")))
        (draw-object! coin-adt tile)))

    (define (dedraw-coin! coin-adt)
      (dedraw-object static-layer coin-tiles coin-adt))

    ;;
    ;; Power-up
    ;; draw-powerup! :: coin -> /
    (define (draw-powerup! powerup-adt)
      (let ((tile (get-object 'powerup powerup-adt powerup-tiles "images/powerup.png" "images/powerup_mask.png")))
        (draw-object! powerup-adt tile)))

    
    ;; dedraw-powerup! :: powerup -> /
    (define (dedraw-powerup! powerup-adt)
      (dedraw-object static-layer powerup-tiles powerup-adt))



    ;; fruit
    ;; draw-fruit! :: fruit -> /
    (define (draw-fruit! fruit-adt)
      (let ((fruit-type (fruit-adt 'type))
            (tile '()))
        (cond
          ((eq? 'cherry fruit-type)
           (set! tile (get-object 'fruit fruit-adt fruit-tiles "images/cherry.png" "images/cherry_mask.png"))
           (draw-object! fruit-adt tile))
          ((eq? 'strawberry fruit-type)
           (set! tile (get-object 'fruit fruit-adt fruit-tiles "images/strawberry.png" "images/strawberry_mask.png"))
           (draw-object! fruit-adt tile))
          ((eq? 'orange fruit-type)
           (set! tile (get-object 'fruit fruit-adt fruit-tiles "images/orange.png" "images/orange_mask.png"))
           (draw-object! fruit-adt tile))
          ((eq? 'apple fruit-type)
            (set! tile (get-object 'fruit fruit-adt fruit-tiles "images/apple.png" "images/apple_mask.png"))
           (draw-object! fruit-adt tile))
          ((eq? 'melon fruit-type)
           (set! tile (get-object 'fruit fruit-adt fruit-tiles "images/melon.png" "images/melon_mask.png"))
           (draw-object! fruit-adt tile)))))

    ;;
    ;; fruit
    ;; dedraw-fruit! :: fruit -> /
    (define (dedraw-fruit! fruit-adt)
      (dedraw-object static-layer fruit-tiles fruit-adt)
      (set! fruit-tiles '()))

        
        
    ;;
    ;; Moveables
    ;; draw-moveable :: pacman-adt or ghost-adt
    (define (draw-moveable! moveable-adt)
      (cond
        ((pacman? (moveable-adt 'type))(draw-pacman! moveable-adt))
        ((ghost? (moveable-adt 'type))(draw-ghost! moveable-adt))
        ((scared-ghost? (moveable-adt 'type))(draw-scared-ghost! moveable-adt))
        (else "not a moveable-adt")))

    

    ;;
    ;; Pacman
    ;; draw-pacman! :: pacman -> /
    (define (draw-pacman! pacman-adt)
      (let ((tile (get-object 'pacman pacman-adt pacman-tiles "images/pacman-3.png" "images/pacman-3_mask.png")))
        (draw-object! pacman-adt tile)))
                         
                
    ;;
    ;; ghost
    ;; draw-ghost :: ghost -> /
    (define (draw-ghost! ghost-adt)
      (dedraw-all! scared-ghost-tiles ghost-layer)
      (set! scared-ghost-tiles '())
      (let ((ghost-type (ghost-adt 'type))
            (tile '()))
        (cond
          ((eq? 'red-ghost ghost-type)
           (set! tile (get-object 'ghost ghost-adt ghost-tiles "images/red-ghost.png" "images/red-ghost_mask.png"))
           (draw-object! ghost-adt tile))
          ((eq? 'blue-ghost ghost-type)
           (set! tile (get-object 'ghost ghost-adt ghost-tiles "images/blue-ghost.png" "images/blue-ghost_mask.png"))
           (draw-object! ghost-adt tile))
          ((eq? 'orange-ghost ghost-type)
           (set! tile (get-object 'ghost ghost-adt ghost-tiles "images/orange-ghost.png" "images/orange-ghost_mask.png"))
           (draw-object! ghost-adt tile))
          ((eq? 'pink-ghost ghost-type)
           (set! tile (get-object 'ghost ghost-adt ghost-tiles "images/pink-ghost.png" "images/pink-ghost_mask.png"))
           (draw-object! ghost-adt tile)))))

    (define (draw-scared-ghost! ghost-adt)
      (dedraw-all! ghost-tiles ghost-layer)
      (set! ghost-tiles '())
      (let ((tile (get-object 'scared-ghost ghost-adt scared-ghost-tiles "images/scared-ghost.png" "images/scared-ghost_mask.png")))
        (draw-object! ghost-adt tile)))
            

    
    ;; draw-scoreboard! :: scoreboard -> /
    (define (draw-scoreboard! scoreboard-adt)
      (draw-score! scoreboard-adt)
      (draw-highscore! scoreboard-adt)
      (draw-lives! scoreboard-adt))
  
    ;; draw-score! :: scoreboard -> /
    (define (draw-score! scoreboard-adt)
      (score-tile 'clear)
      (score-value-tile 'clear)
      ((score-tile 'draw-text) "SCORE:" 15 (car score-position) (cdr score-position) "white")
      ((score-value-tile 'draw-text)(number->string (scoreboard-adt 'score)) 15 (car score-value-position) (cdr score-value-position) "white")
      ((score-layer 'add-drawable) score-tile)
      ((score-layer 'add-drawable) score-value-tile))

    ;; draw-highscore! :: scoreboard -> /
    (define (draw-highscore! scoreboard-adt)
      (highscore-tile 'clear)
      (highscore-value-tile 'clear)
      ((highscore-tile 'draw-text) "HIGHSCORE:" 15 (car highscore-position) (cdr highscore-position) "white")
      ((highscore-value-tile 'draw-text)(number->string (scoreboard-adt 'highscore)) 15 (car highscore-value-position) (cdr highscore-value-position) "white")
      ((score-layer 'add-drawable) highscore-tile)
      ((score-layer 'add-drawable) highscore-value-tile))


    ;; draw-lives! :: scoreboard -> /
     (define (draw-lives! scoreboard-adt)
      (lives-tile 'clear)
      (lives-value-tile 'clear)
      ((lives-tile 'draw-text) "LIVES:" 15 (car lives-position) (cdr lives-position) "white")
      ((lives-value-tile 'draw-text)(number->string (scoreboard-adt 'lives)) 15 (car lives-value-position) (cdr lives-value-position) "white")
      ((score-layer 'add-drawable) lives-tile)
      ((score-layer 'add-drawable) lives-value-tile))
    
                             
    
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
        ((eq? msg 'draw-static!)draw-static!)
        ((eq? msg 'draw-edible!)draw-edible!)
        ((eq? msg 'dedraw-edible!)dedraw-edible!)
        ((eq? msg 'draw-moveable!)draw-moveable!)
        ((eq? msg 'draw-scoreboard!)draw-scoreboard!)
        ((eq? msg 'clear-all!)clear-all!)))
        
        dispatch-draw-adt))