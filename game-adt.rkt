;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Level-adt.rkt")
(load "Draw-adt.rkt")
(load "Scoreboard-adt.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Game ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-game-adt :: / -> game
(define (make-game-adt)
  (let* ((level 1)
         (level-adt (make-level-adt true-game-width true-game-height level))
         (draw-adt (make-draw-adt window-width-px window-height-px))
         (game-score (make-scoreboard-adt)))
    
    ;; key-procedure :: symbol, any -> /
    (define (key-procedure status key)
      ;; status is equal to:
      ;; 'pressed: when the key is pressed
      ;; 'released: when the key is released
      ;; 'when pressed for a longer duration then the procedure is recalled multiple times for the same key
      
      (if (eq? status 'pressed)
          ((level-adt 'key!) key)))

    ;; make-new-level! :: / -> /
    (define (make-new-level!)
      (let ((new-level-adt (make-level-adt true-game-width true-game-height level)))
        (set! level-adt new-level-adt)
        ((level-adt 'update-highscore!)(game-score 'highscore))
        ((level-adt 'update-score!)(game-score 'score))
        ((level-adt 'update-lives!)(game-score 'lives))))

    
    ;; next-level! :: / -> /
    (define (next-level!)
      (set! level (+ level 1))
      ((game-score 'update-lives!)(level-adt 'lives))
      ((game-score 'update-score!)(level-adt 'score))
      ((game-score 'update-highscore!)(game-score 'score))
      ((draw-adt 'clear-all!))
      (make-new-level!))

    
    ;; reset-level! :: / -> /
    (define (reset-level!)
      (set! level one)
      ((game-score 'update-lives!)starting-lives)
      ((game-score 'update-score!)(level-adt 'score))
      ((game-score 'update-highscore!)(game-score 'score))
      ((game-score 'reset-score!))
      ((draw-adt 'clear-all!))
      (make-new-level!))
    
    ;; game-loop-procedure :: number -> /
    (define (game-loop-procedure delta-tijd)
      (cond
        ((zero? (level-adt 'coins-counter))(next-level!))
        ((zero? (level-adt 'lives))(reset-level!))
        (else
           ((level-adt 'update!) delta-tijd)
           ((level-adt 'draw!) draw-adt))))

    
    ;; procedure to start the game
    ;; start :: / -> /
    (define (start)
      ;; sets the callbacks through the draw-adt
      ((draw-adt 'set-game-loop-procedure!) game-loop-procedure)
      ((draw-adt 'set-key-procedure!) key-procedure))
    
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-game msg)
      (cond ((eq? msg 'start) start)))
    dispatch-game))
