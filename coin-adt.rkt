;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Edible-adt.rkt")
(load "Draw-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Coin ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-coin-adt coin-position)
  (let ((edible-adt (make-edible-adt coin-position 'coin)))
    ((edible-adt 'set-score-value!)1)
    ((edible-adt 'drop!))
    ;; 
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;

    ;;
    ;; Dispatch
    ;;
    (define (coin-dispatch msg)
      (cond
        (else (edible-adt msg))))
    coin-dispatch))
