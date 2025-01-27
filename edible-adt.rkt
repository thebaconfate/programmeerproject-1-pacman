#lang r5rs

(#%require "static-adt.rkt")
(#%provide make-edible-adt)


(define (make-edible-adt x y type score)
  (let ((static (make-static-adt x y type)))

    (define dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-score) score)
          (else (static message)))))

    dispatch))


