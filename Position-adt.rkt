;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               position ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;NOTE: credits -> oplossingen van snake WPO. 


;; maak-position-adt :: number, number -> position
(define (make-position-adt x y)


  ;; x! :: number -> /
  (define (x! new-x)
    (set! x new-x))

  ;; y! :: number -> /
  (define (y! new-y)
    (set! y new-y))

  ;; compare-position? :: position -> boolean
  (define (compare-position? other-position)
    (and (= x (other-position 'x))
         (= y (other-position 'y))))


  ;; move calls the make-position-adt procedure and essentially makes a new adt with positions x and y
  ;; but with one variable changed with 1. respectively in which direction the position of the entity
  ;; moved HOWEVER this one is NOT destructive
  ;; move! :: string -> /
  (define (next-position direction)
    (cond ((eq? direction 'up) (make-position-adt x (- y 1)))
          ((eq? direction 'down) (make-position-adt x (+ y 1)))
          ((eq? direction 'left) (make-position-adt (- x 1) y))
          ((eq? direction 'right) (make-position-adt (+ x 1) y))))



    (define (dispatch-position msg)
    (cond ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'next-position) next-position)
          ((eq? msg 'compare-position?) compare-position?)))
  dispatch-position)



  
  