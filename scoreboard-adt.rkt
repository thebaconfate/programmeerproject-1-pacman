;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            scoreboard ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-scoreboard-adt)
  (let* ((score 0)
         (highscore 0)
         (lives 3))


    
    
    ;; update-highscore! :: / -> /
    (define (update-highscore! val)
        (if (> val highscore)
            (set! highscore val)))
    
    ;; add-score! :: number -> /
    (define (add-score! value)
      (set! score (+ score value)))
      
    ;; reset-score! :: integer -> /
    (define (reset-score!)
      (set! score 0))

    ;; update-score! :: integer -> /
    (define (update-score! val)
      (set! score val))

    ;; update-lives! :: integer -> /
    (define (update-lives! new-lives)
      (set! lives new-lives))
    
    ;; draw! :: draw-adt -> /
    (define (draw! draw-adt)
      ((draw-adt 'draw-scoreboard!)dispatch-scoreboard))     
    
    
    (define (dispatch-scoreboard msg)
      (cond
        ((eq? msg 'score)score)
        ((eq? msg 'highscore)highscore)
        ((eq? msg 'lives)lives)
        ((eq? msg 'update-score!)update-score!)
        ((eq? msg 'add-score!)add-score!)
        ((eq? msg 'update-highscore!)update-highscore!)
        ((eq? msg 'update-lives!)update-lives!)
        ((eq? msg 'reset-score!)reset-score!)
        ((eq? msg 'draw!)draw!)))
    dispatch-scoreboard))