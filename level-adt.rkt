;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(#%require "Graphics.rkt")
;(load "Constants.rkt")
;(load "Position-adt.rkt")
;(load "Grid-adt.rkt")
;(load "Wall-adt.rkt")
;(load "Pacman-adt.rkt")
;(load "Coin-adt.rkt")
;(load "Fruit-adt.rkt")
;(load "Scoreboard-adt.rkt")
;(load "Gate-adt.rkt")
;(load "Powerup-adt.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-adt-level :: number, number -> level
(define (make-level-adt amount-of-cells-width amount-of-cells-height level)
  (let* ((left-teleport (make-position-adt out-of-bounds-left teleport-y))
         (right-teleport (make-position-adt out-of-bounds-right teleport-y))
         (walls-adt (make-grid-adt amount-of-cells-width amount-of-cells-height))
         (coins-adt (make-grid-adt amount-of-cells-width amount-of-cells-height))
         (level-score (make-scoreboard-adt))
         (coins-counter 0)
         (pacman-adt (make-pacman-adt (make-position-adt start-value-x-pacman start-value-y-pacman)))
         (pacman-time 0)
         (red-ghost-adt (make-ghost-adt (make-position-adt (car ghost-in-box)(cadr ghost-in-box))))
         (blue-ghost-adt (make-ghost-adt (make-position-adt (car ghost-in-box)(cadr ghost-in-box))))
         (orange-ghost-adt (make-ghost-adt (make-position-adt (car ghost-in-box)(cadr ghost-in-box))))
         (pink-ghost-adt (make-ghost-adt (make-position-adt (car ghost-in-box)(cadr ghost-in-box))))
         (ghost-time 0)
         (released-ghosts (make-vector 4 #f))
         (released-time 0)
         (invincible #f)
         (invincible-time 0)
         (fruit-time 0)
         (fruit-adt #f))

    (define (write-rows! write-object peek-object from-lst till-lst height-lst)
      (if (and (not (null? from-lst))
               (not (null? till-lst))
               (not (null? height-lst)))
          (let ((from-val (car from-lst))
                (till-val (car till-lst))
                (height-val (car height-lst)))
            (let loop ((counter from-val))
              (if (> counter till-val)
                  (write-rows! write-object peek-object(cdr from-lst)(cdr till-lst)(cdr height-lst))
                  (if (eq? (peek-object counter height-val) free-spot)
                      (begin (write-object counter height-val)
                             (loop (+ counter game-step)))
                      (loop (+ counter game-step))))))))
    
    
    
    (define (write-columns! write-object peek-object from-lst till-lst width-lst)
      (if (and (not (null? from-lst))
               (not (null? till-lst))
               (not (null? width-lst)))
          (let ((from-val (car from-lst))
                (till-val (car till-lst))
                (width-val (car width-lst)))
            (let loop ((counter from-val))
              (if (> counter till-val)
                  (write-columns! write-object peek-object (cdr from-lst)(cdr till-lst)(cdr width-lst))
                  (if (eq? (peek-object width-val counter) free-spot)
                      (begin (write-object width-val counter)
                             (loop (+ counter game-step)))
                      (loop (+ counter game-step))))))))

    (define (write-one! width height)
      ((walls-adt 'write!) width height one))



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               Walls part                               ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;; peek-wall : integer integer -> vct
    (define (peek-wall width height)
      ((walls-adt 'peek) width height))

    
    ;; free? :: position -> boolean
    (define (free? position)
      (or (eq? (peek-wall (position 'x)(position 'y)) free-spot)
          (eq? (peek-wall (position 'x)(position 'y)) one)))

    
    ;; write-wall! :: integer integer -> /
    (define (write-wall! width height)
      ((walls-adt 'write!)width height (make-wall-adt (make-position-adt width height))))


    ;; initialize-level-1-walls :: / -> /
    (define (initialize-level-1-walls)
      (write-rows! write-wall! peek-wall level-1-wall-list-of-start-x level-1-wall-list-of-end-x level-1-wall-list-of-row-y)
      (write-columns! write-wall! peek-wall level-1-wall-list-of-start-y level-1-wall-list-of-end-y level-1-wall-list-of-column-x))


    ;; initialize-level-1-walls :: / -> /
    (define (initialize-level-2-walls)
      (write-rows! write-wall! peek-wall level-2-wall-list-of-start-x level-2-wall-list-of-end-x level-2-wall-list-of-row-y)
      (write-columns! write-wall! peek-wall level-2-wall-list-of-start-y level-2-wall-list-of-end-y level-2-wall-list-of-column-x))


    ;; initialize-level-1-walls :: / -> /
    (define (initialize-level-3-walls)
      (write-rows! write-wall! peek-wall level-3-wall-list-of-start-x level-3-wall-list-of-end-x level-3-wall-list-of-row-y)
      (write-columns! write-wall! peek-wall level-3-wall-list-of-start-y level-3-wall-list-of-end-y level-3-wall-list-of-column-x))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               Gate part                                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; write-wall! :: integer integer -> /
    (define (write-gate! width height)
      ((walls-adt 'write!)width height (make-gate-adt (make-position-adt width height))))

    (define (initialize-gates)
      (write-rows! write-gate! peek-wall gate-list-of-start-x gate-list-of-end-x gate-list-of-row-y)
      (write-rows! write-one! peek-wall  one-list-of-start-x one-list-of-end-x one-list-of-row-y))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               Coin part                                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;; peek-coin :: integer integer -> vct
    (define (peek-coin width height)
      ((coins-adt 'peek) width height))


    ;; write-coin! :: integer integer -> /
    (define (write-coin! width height)
      ((coins-adt 'write!)width height (make-coin-adt (make-position-adt width height))))
    
    ;; initialize-level-1-coins :: / -> /
    (define (initialize-level-1-coins)
      (write-rows! write-coin! peek-coin level-1-coin-list-of-start-x level-1-coin-list-of-end-x level-1-coin-list-of-row-y)
      (write-columns! write-coin! peek-coin level-1-coin-list-of-start-y level-1-coin-list-of-end-y level-1-coin-list-of-column-x))


    ;; initialize-level-1-coins :: / -> /
    (define (initialize-level-2-coins)
      (write-rows! write-coin! peek-coin level-2-coin-list-of-start-x level-2-coin-list-of-end-x level-2-coin-list-of-row-y)
      (write-columns! write-coin! peek-coin level-2-coin-list-of-start-y level-2-coin-list-of-end-y level-2-coin-list-of-column-x))


    ;; initialize-level-1-coins :: / -> /
    (define (initialize-level-3-coins)
      (write-rows! write-coin! peek-coin level-3-coin-list-of-start-x level-3-coin-list-of-end-x level-3-coin-list-of-row-y)
      (write-columns! write-coin! peek-coin level-3-coin-list-of-start-y level-3-coin-list-of-end-y level-3-coin-list-of-column-x))

    ;; is-coin? :: position -> boolean
    (define (is-coin? position)
      (if (eq? (peek-coin (position 'x)(position 'y)) free-spot)
          #f
          (and ((peek-coin (position 'x)(position 'y))'on-the-ground?)
               (coin? ((peek-coin (position 'x)(position 'y))'type)))))


    ;; eat-coin! :: position -> /
    (define (eat! position)
      (((peek-coin (position 'x) (position 'y))'eat!)))
      
    

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                              powerup part                              ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;; write-powerup! :: integer integer -> /
    (define (write-powerup! width height)
      ((coins-adt 'write!) width height (make-powerup-adt (make-position-adt width height))))


    ;; write-powerups! :: list -> /
    (define (write-powerups! list-of-positions)
      (let loop ((lst list-of-positions))
        (if (not (null? lst))
            (begin (write-powerup! (caar lst) (cadar lst))
                   (loop (cdr lst))))))
    
    
    ;; is-coin? :: position -> boolean
    (define (is-powerup? position)
      (if (eq? (peek-coin (position 'x)(position 'y)) free-spot)
          #f
          (and ((peek-coin (position 'x)(position 'y))'on-the-ground?)
               (powerup? ((peek-coin (position 'x)(position 'y))'type)))))

    
    ;; initialize-level-1-powerups :: / -> /
    (define (initialize-level-1-powerups)
      (write-powerups! level-1-powerups))
   

    ;; initialize-level-1-powerups :: / -> /
    (define (initialize-level-2-powerups)
      (write-powerups! level-2-powerups))

    ;; initialize-level-1-powerups :: / -> /
    (define (initialize-level-3-powerups)
      (write-powerups! level-3-powerups))


    ;; update-invincibility
    (define (update-invincibility)
      (if (and invincible (> invincible-time invincibility-speed))
          (begin (set! invincible #f)
                 (initialize-ghosts))))
          


    

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               fruit part                               ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (free-for-fruit? position)
      (eq? (peek-wall (position 'x)(position 'y)) free-spot))

    (define (make-fruit-position x y)
      (let ((possible-position (make-position-adt x y)))
        (cond
          ((and (>= x fruit-x-min)(>= y fruit-y-min))
           (if (free-for-fruit? possible-position)
               possible-position
               (make-fruit-position (random fruit-x-max)(random fruit-y-max))))
          (else (make-fruit-position (random fruit-x-max)(random fruit-y-max))))))
     
    (define (initialize-fruit!)
      (set! fruit-adt (make-fruit-adt (make-fruit-position (random fruit-x-max)(random fruit-y-max))))
      ((fruit-adt 'make-a-fruit!)(random fruit-generator))
      (set! fruit-time 0))

    (define (pick-up-fruit!)
      ((fruit-adt 'eat!)))

    (define (update-fruit!)
      (if fruit-adt
          (if (fruit-adt 'on-the-ground?)
              (if (> fruit-time fruit-speed)
                  (pick-up-fruit!)))
          (if (> fruit-time fruit-speed)
              (initialize-fruit!))))


    (define (fruit? position)
      ((position 'compare-position?)(fruit-adt 'position)))

    (define (eat-fruit!)
      (pick-up-fruit!))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               Ghost part                               ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (define (write-ghost! which home-position)
      (vector-set! ghost-adts which (make-ghost-adt home-position)))
    
      


    
    (define (release-ghost!)
      (cond
        ((not (vector-ref released-ghosts red-ghost-ref))(((red-ghost-adt 'position)'x!)(car ghost-spawn))
                                                         (((red-ghost-adt 'position)'y!)(cadr ghost-spawn))
                                                         (vector-set! released-ghosts red-ghost-ref #t))
        ((not (vector-ref released-ghosts blue-ghost-ref))(((blue-ghost-adt 'position)'x!)(car ghost-spawn))
                                                          (((blue-ghost-adt 'position)'y!)(cadr ghost-spawn))
                                                          (vector-set! released-ghosts blue-ghost-ref #t))
        ((not (vector-ref released-ghosts orange-ghost-ref))(((orange-ghost-adt 'position)'x!)(car ghost-spawn))
                                                            (((orange-ghost-adt 'position)'y!)(cadr ghost-spawn))
                                                            (vector-set! released-ghosts orange-ghost-ref #t))
        ((not (vector-ref released-ghosts pink-ghost-ref))(((pink-ghost-adt 'position)'x!)(car ghost-spawn))
                                                          (((pink-ghost-adt 'position)'y!)(cadr ghost-spawn))
                                                          (vector-set! released-ghosts pink-ghost-ref #t))))
    

    (define (update-ghosts!)
      (if (> released-time released-speed)
          (begin (release-ghost!)
                 (set! released-time 0))))

    (define (reset-ghost-timer!)
      (set! ghost-time 0))

    (define (kill-pacman!)
      (let ((new-x start-value-x-pacman)
            (new-y start-value-y-pacman))
        ((level-score 'add-score!)(pacman-adt 'score))
        ((level-score 'update-lives!)(- (level-score 'lives) 1))
        (reinitialize-all-ghosts)
        (((pacman-adt 'position)'x!)new-x)
        (((pacman-adt 'position)'y!)new-y)))

    (define (kill-ghost! ghost-adt)
      ((level-score 'add-score!)(ghost-adt 'score))
      (reinitialize-ghost ghost-adt))

    (define (killable? ghost-adt)
      (((ghost-adt 'position) 'compare-position?) (pacman-adt 'position)))
        
         

    (define (attempt-to-kill-pacman ghost-adt)
      (if (killable? ghost-adt)
          (if invincible
              (kill-ghost! ghost-adt)
              (kill-pacman!))))

    ;;
    ;; red-ghost behavior
    ;;


    (define (move-red-ghost!)
      (let* ((current-position (red-ghost-adt 'position))
             (next-position ((current-position 'next-position)(red-ghost-adt 'direction))))
        (cond
          ((or((next-position 'compare-position?)left-teleport)
              ((next-position 'compare-position?)right-teleport))
           ((red-ghost-adt 'teleport!))
           (attempt-to-kill-pacman red-ghost-adt))
          ((free? next-position)
           ((red-ghost-adt 'move!))
           (attempt-to-kill-pacman red-ghost-adt))
          (else (update-red-ghost-direction!)))))
    
    (define (update-red-ghost-direction!)
      (let ((current-direction (red-ghost-adt 'direction)))
        (cond
          ((eq? 'up current-direction)((red-ghost-adt 'direction!)'left)
                                      (move-red-ghost!))
          ((eq? 'left current-direction)((red-ghost-adt 'direction!)'down)
                                        (move-red-ghost!))
          ((eq? 'down current-direction)((red-ghost-adt 'direction!)'right)
                                        (move-red-ghost!))
          ((eq? 'right current-direction)((red-ghost-adt 'direction!)'up)
                                         (move-red-ghost!)))))

    ;;
    ;; blue-ghost behavior
    ;;

    (define (move-blue-ghost!)
      (let* ((current-position (blue-ghost-adt 'position))
             (next-position ((current-position 'next-position)(blue-ghost-adt 'direction))))
        (cond
          ((or((next-position 'compare-position?)left-teleport)
              ((next-position 'compare-position?)right-teleport))
           ((blue-ghost-adt 'teleport!))
           (attempt-to-kill-pacman blue-ghost-adt))
          ((free? next-position)
           ((blue-ghost-adt 'move!))
           (attempt-to-kill-pacman blue-ghost-adt))
          (else (update-blue-ghost-direction!)))))

  
    

    (define (update-blue-ghost-direction!)
      (let ((current-direction (blue-ghost-adt 'direction)))
        (cond
          ((eq? 'up current-direction)((blue-ghost-adt 'direction!)'right)
                                      (move-blue-ghost!))
          ((eq? 'left current-direction)((blue-ghost-adt 'direction!)'up)
                                        (move-blue-ghost!))
          ((eq? 'down current-direction)((blue-ghost-adt 'direction!)'left)
                                        (move-blue-ghost!))
          ((eq? 'right current-direction)((blue-ghost-adt 'direction!)'down)
                                         (move-blue-ghost!)))))
    ;;
    ;; orange-ghost behavior
    ;;

    (define (move-orange-ghost!)
      (let* ((current-position (orange-ghost-adt 'position))
             (next-position ((current-position 'next-position)(orange-ghost-adt 'direction))))
        (cond
          ((or((next-position 'compare-position?)left-teleport)
              ((next-position 'compare-position?)right-teleport))
           ((orange!-ghost-adt 'teleport!))
           (attempt-to-kill-pacman orange-ghost-adt))
          ((free? next-position)
           ((orange-ghost-adt 'move!))
           (attempt-to-kill-pacman orange-ghost-adt))
          (else (update-orange-ghost-direction!)))))

    

    (define (update-orange-ghost-direction!)
      (let* ((directions 4)
             (random-direction (random directions)))
        (cond
          ((= 0 random-direction)((orange-ghost-adt 'direction!)'right)
                                      (move-orange-ghost!))
          ((= 1 random-direction)((orange-ghost-adt 'direction!)'up)
                                        (move-orange-ghost!))
          ((= 2 random-direction)((orange-ghost-adt 'direction!)'left)
                                        (move-orange-ghost!))
          ((= 3 random-direction)((orange-ghost-adt 'direction!)'down)
                                         (move-orange-ghost!)))))

    ;;
    ;; pink-ghost behavior
    ;;

    (define (move-pink-ghost!)
      (let* ((current-position (pink-ghost-adt 'position))
             (next-position ((current-position 'next-position)(pink-ghost-adt 'direction))))
        (cond
          ((or((next-position 'compare-position?)left-teleport)
              ((next-position 'compare-position?)right-teleport))
           ((pink-ghost-adt 'teleport!))
           (attempt-to-kill-pacman pink-ghost-adt))
          ((free? next-position)
           ((pink-ghost-adt 'move!))
           (attempt-to-kill-pacman pink-ghost-adt)
           (update-pink-ghost-direction! #f))
          (else (update-pink-ghost-direction! #t)))))

    

    (define (update-pink-ghost-direction! boolean)
      (let* ((total-chance 101)
             (chance-1 25)
             (chance-2 50)
             (chance-3 65)
             (chance-4 100)
             (random-chance (random total-chance)))
        (cond
          ((<= random-chance chance-1)((pink-ghost-adt 'direction!)'right)
                                      (if boolean
                                          (move-pink-ghost!)))
          ((and (> random-chance chance-2)
                (<= random-chance chance-3))((pink-ghost-adt 'direction!)'up)
                                            (if boolean
                                                (move-pink-ghost!)))
          ((and (> random-chance chance-1)
                (<= random-chance chance-2))((pink-ghost-adt 'direction!)'left)
                                            (if boolean
                                                (move-pink-ghost!)))
          ((and (> random-chance chance-3)
                (<= random-chance chance-4))((pink-ghost-adt 'direction!)'down)
                                            (if boolean
                                                (move-pink-ghost!))))))

  
    (define (make-ghosts-scared)
      ((red-ghost-adt 'make-scared-ghost!))
      ((blue-ghost-adt 'make-scared-ghost!))
      ((orange-ghost-adt 'make-scared-ghost!))
      ((pink-ghost-adt 'make-scared-ghost!)))
                    

    (define (move-all-ghosts!)
      (move-red-ghost!)
      (move-blue-ghost!)
      (move-orange-ghost!)
      (move-pink-ghost!)
      (reset-ghost-timer!))

    
    (define (move-ghosts!)
      (let ((half-speed (* ghost-speed 2)))
        (if invincible
            (if (> ghost-time half-speed)
                (move-all-ghosts!))
            (if (> ghost-time ghost-speed)
                (move-all-ghosts!)))))
                
    
    (define (reinitialize-ghost ghost-adt)
      (let ((new-x (car ghost-in-box))
            (new-y (cadr ghost-in-box)))
        (cond
          ((eq? (ghost-adt 'colour) 'red)
           (((red-ghost-adt 'position)'x!)new-x)
           (((red-ghost-adt 'position)'y!)new-y)
           (vector-set! released-ghosts red-ghost-ref #f))
          ((eq? (ghost-adt 'colour) 'blue)
           (((blue-ghost-adt 'position)'x!)new-x)
           (((blue-ghost-adt 'position)'y!)new-y)
           (vector-set! released-ghosts blue-ghost-ref #f))
          ((eq? (ghost-adt 'colour) 'orange)
           (((orange-ghost-adt 'position)'x!)new-x)
           (((orange-ghost-adt 'position)'y!)new-y)
           (vector-set! released-ghosts orange-ghost-ref #f))
          ((eq? (ghost-adt 'colour) 'pink)
           (((pink-ghost-adt 'position)'x!)new-x)
           (((pink-ghost-adt 'position)'y!)new-y)
           (vector-set! released-ghosts pink-ghost-ref #f)))))
        

    
    (define (reinitialize-all-ghosts)
      (let ((new-x (car ghost-in-box))
            (new-y (cadr ghost-in-box)))
        (((red-ghost-adt 'position)'x!)new-x)
        (((red-ghost-adt 'position)'y!)new-y)
        (vector-set! released-ghosts red-ghost-ref #f)
        (((blue-ghost-adt 'position)'x!)new-x)
        (((blue-ghost-adt 'position)'y!)new-y)
        (vector-set! released-ghosts blue-ghost-ref #f)
        (((orange-ghost-adt 'position)'x!)new-x)
        (((orange-ghost-adt 'position)'y!)new-y)
        (vector-set! released-ghosts orange-ghost-ref #f)
        (((pink-ghost-adt 'position)'x!)new-x)
        (((pink-ghost-adt 'position)'y!)new-y)
        (vector-set! released-ghosts pink-ghost-ref #f)))
        


        
        


    (define (initialize-ghosts)
      ((red-ghost-adt 'make-red-ghost!))
      ((blue-ghost-adt 'make-blue-ghost!))
      ((blue-ghost-adt 'direction!)'right)
      ((orange-ghost-adt 'make-orange-ghost!))
      ((orange-ghost-adt 'direction!)'left)
      ((pink-ghost-adt 'make-pink-ghost!)))

      
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                               Pacman part                              ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    ;; reset-pacman-timer! :: / -> /
    (define (reset-pacman-timer!)
      (set! pacman-time 0))

    ;; reset-pacman-timer! :: / -> /
    (define (reset-invincibility-timer!)
      (set! invincible-time 0))

    (define (pacman-routine)
      ((pacman-adt 'move!))
      (reset-pacman-timer!)
      (attempt-to-kill-pacman red-ghost-adt)
      (attempt-to-kill-pacman blue-ghost-adt)
      (attempt-to-kill-pacman orange-ghost-adt)
      (attempt-to-kill-pacman pink-ghost-adt)
      (if (is-coin? (pacman-adt 'position))
          (begin
            ((level-score 'add-score!)((peek-coin ((pacman-adt 'position)'x) ((pacman-adt 'position)'y))'score)) 
            (eat! (pacman-adt 'position))
            (set! coins-counter (- coins-counter 1)))
          (if (is-powerup? (pacman-adt 'position))
              (begin
                (eat! (pacman-adt 'position))
                (set! invincible #t)
                (make-ghosts-scared)
                (reset-invincibility-timer!))
              (if fruit-adt
                  (if (fruit? (pacman-adt 'position))
                      (begin
                        ((level-score 'add-score!)(fruit-adt 'score)) 
                        (eat-fruit!)))))))

    (define (pacman-teleport!)
      ((pacman-adt 'teleport!))
      (reset-pacman-timer!))
    
    (define (move-pacman!)
      (if (> pacman-time pacman-speed)
          (let* ((current-position (pacman-adt 'position))
                 (next-position ((current-position 'next-position)(pacman-adt 'direction))))
            (cond
              ((or((next-position 'compare-position?)left-teleport)
                  ((next-position 'compare-position?)right-teleport))
               (pacman-teleport!))
              ((free? next-position)
               (pacman-routine))))))
            

    (define (turn-pacman! key)
      ((pacman-adt 'direction!)key))

    
         
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                          General game logic                            ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    ;; update! :: number -> /
    (define (update! delta-time)
      ;;NOTE: Add new times according to new adts used in the level adt
      (set! fruit-time (+ fruit-time delta-time))
      (set! pacman-time (+ pacman-time delta-time))
      (set! released-time (+ released-time delta-time))
      (set! ghost-time (+ ghost-time delta-time))
      (set! invincible-time (+ invincible-time delta-time))
      ;;NOTE: Add new move-object functions
      (update-fruit!)
      (update-invincibility)
      (move-pacman!)
      (move-ghosts!)
      (update-ghosts!))

    ;; key! :: any -> /
    (define (key! key)
      (cond
        ((direction? key )(turn-pacman! key))
        (else (display key))))
    
    ;; draw! :: draw -> /
    (define (draw! draw-adt)
      ((walls-adt 'draw-matrix!)draw-adt)
      ((pacman-adt 'draw!) draw-adt)
      ((coins-adt 'draw-matrix!)draw-adt)
      ((level-score 'draw!)draw-adt)
      ((red-ghost-adt'draw!)draw-adt)
      ((blue-ghost-adt'draw!)draw-adt)
      ((orange-ghost-adt'draw!)draw-adt)
      ((pink-ghost-adt'draw!)draw-adt)
      (if fruit-adt
          ((fruit-adt 'draw!)draw-adt)))
      

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                            Initialisation                              ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define (initialize-level-1)
      (initialize-level-1-walls)
      (initialize-gates)
      (initialize-level-1-coins)
      (set! coins-counter ((coins-adt 'count-objects)))
      (initialize-level-1-powerups)
      (initialize-ghosts))


    (define (initialize-level-2)
      (initialize-level-2-walls)
      (initialize-gates)
      (initialize-level-2-coins)
      (set! coins-counter ((coins-adt 'count-objects)))
      (initialize-level-2-powerups)
      (initialize-ghosts))

    (define (initialize-level-3)
      (initialize-level-3-walls)
      (initialize-gates)
      (initialize-level-3-coins)
      (set! coins-counter ((coins-adt 'count-objects)))
      (initialize-level-3-powerups)
      (initialize-ghosts))
    

    (define (initialize-level level)
      (cond
        ((= level 1)(initialize-level-1))
        ((= level 2)(initialize-level-2))
        ((>= level 3)(initialize-level-3))))


    (initialize-level level)


    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'draw!) draw!)
            ((eq? msg 'key!) key!)
            ((eq? msg 'coins-counter)coins-counter)
            (else (level-score msg))))
    dispatch-level))