(load "Position-adt.rkt")
(load "Wall-adt.rkt")

(define (nigger data)
    (if (boolean? data)
        data
        (data 'passable?)))

(define wall-adt (make-wall-adt (make-position-adt 1 1) 'horizontal))

(nigger #t)
(nigger wall-adt)