;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(load "Constants.rkt")
(load "Position-adt.rkt")
(load "Draw-adt.rkt")
(load "Static-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Gate ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-gate-adt gate-position)
  (let ((static-adt (make-static-adt gate-position 'gate)))
    ;;
    ;; private-procedures
    ;;

    ;;
    ;; public-procedures
    ;;
    
    (define (dispatch-gate msg)
      (cond
        (else (static-adt msg))))
    dispatch-gate))
