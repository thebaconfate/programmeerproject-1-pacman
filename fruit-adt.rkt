;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Position-adt.rkt")
(load "Edible-adt.rkt")
(load "Draw-adt.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Fruit ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (make-fruit-adt fruit-position)
  (let ((edible-adt (make-edible-adt fruit-position 'fruit)))

    ;;
    ;; private-procedures
    ;;
    
    (define (within-bounds? val left-bound right-bound)
      (and (>= val left-bound)
           (< val right-bound)))
        
    ;;
    ;; public-procedures
    ;;
    
    
    ;; make-cherry :: / -> /
    (define (make-cherry!)
      ((edible-adt 'type!) 'cherry)
      ((edible-adt 'set-score-value!) cherry-value))

    ;; make-strawberry :: / -> /
    (define (make-strawberry!)
      ((edible-adt 'type!) 'strawberry)
      ((edible-adt 'set-score-value!) strawberry-value))
    
    ;; make-orange :: / -> /
    (define (make-orange!)
      ((edible-adt 'type!) 'orange)
      ((edible-adt 'set-score-value!) orange-value))


    ;; make-apple :: / -> /
    (define (make-apple!)
      ((edible-adt 'type!) 'apple)
      ((edible-adt 'set-score-value!) apple-value))
    

    ;; make-melon :: / -> /
    (define (make-melon!)
      ((edible-adt 'type!) 'melon)
      ((edible-adt 'set-score-value!) melon-value))

    ;; make-a-fruit :: number -> /
    (define (make-a-fruit! value)
      (let ((lowest-bound 0)
            (cherry-bound 30)
            (strawberry-bound 55)
            (orange-bound 75)
            (apple-bound 90)
            (melon-bound 100))
        (cond
          ((within-bounds? value lowest-bound cherry-bound)(make-cherry!)((edible-adt 'drop!)))
          ((within-bounds? value cherry-bound strawberry-bound)(make-strawberry!)((edible-adt 'drop!)))
          ((within-bounds? value strawberry-bound orange-bound)(make-orange!)((edible-adt 'drop!)))
          ((within-bounds? value orange-bound apple-bound)(make-apple!)((edible-adt 'drop!)))
          ((within-bounds? value apple-bound melon-bound)(make-melon!)((edible-adt 'drop!))))))
      

    ;;
    ;; Dispatch
    ;;

    (define (dispatch-fruit msg)
      (cond
        ((eq? msg 'make-a-fruit!)make-a-fruit!)
        ((eq? msg 'make-cherry!)make-cherry!)
        ((eq? msg 'make-strawberry!)make-strawberry!)
        ((eq? msg 'make-orange!)make-orange!)
        ((eq? msg 'make-apple!)make-apple!)
        ((eq? msg 'make-melon!)make-melon!)
        (else (edible-adt msg))))
    dispatch-fruit))

      