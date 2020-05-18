;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")
(load "Static-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Wall ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-wall-adt wall-position)
  (let ((static-adt (make-static-adt wall-position 'wall)))
    ;;
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;
    
    (define (dispatch-wall msg)
      (cond
        (else (static-adt msg))))
    dispatch-wall))