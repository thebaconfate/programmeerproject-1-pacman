#lang r5rs

(#%require "constants.rkt" "help-procedures.rkt" "grid-adt.rkt" "moveable-adt.rkt")
(#%provide make-level-adt)

(define (make-level-adt width height level)
  (let* ((static-grid (make-grid-adt height width)))

    (define level-dispatch
      (lambda (message)
        (cond
          (else (display-invalid-message message "LEVEL-ADT")))))
    level-dispatch))
