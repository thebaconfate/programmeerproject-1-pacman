;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               edibles ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-edible-adt edibles-position type)
  (let ((on-the-ground #f)
        (score-value 0))

    ;; NOTE: be sure to change the type FIRST priot to changing the on-the-ground boolean
    
    ;;
    ;; private-procedures
    ;;

    (define (reset-score-value!)
      (set! score-value 0))

    ;;
    ;; public-procedures
    ;;
    
  
    ;; drop! :: / -> /
    (define (drop!)
      (set! on-the-ground #t))

    ;; pick-up! :: / -> /
    (define (eat!)
      (set! on-the-ground #f)
      (reset-score-value!))

    ;; type! :: any -> /
    (define (type! new-type)
      (set! type new-type))


    ;; set-score! :: number -> //
    (define (set-score-value! value)
      (set! score-value value))

    ;; make-edible! 


    ;; draw! :: draw -> //
    (define (draw! draw-adt)
      (if on-the-ground
          ((draw-adt 'draw-edible!)   dispatch-edible)
          ((draw-adt 'dedraw-edible!)dispatch-edible)))

    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-edible msg)
      (cond
        ((eq? msg 'position)edibles-position)
        ((eq? msg 'type)type)
        ((eq? msg 'type!)type!)
        ((eq? msg 'on-the-ground?)on-the-ground)
        ((eq? msg 'score)score-value)
        ((eq? msg 'drop!)drop!)
        ((eq? msg 'eat!)eat!)
        ((eq? msg 'set-score-value!)set-score-value!)
        ((eq? msg 'draw!)draw!)
        (else "Message not recognised")))
    dispatch-edible))