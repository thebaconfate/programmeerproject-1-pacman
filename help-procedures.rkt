#lang r5rs

(#%provide (all-defined))

(define (list-length>1? lst)
  (not (null? (cdr lst))))

(define (list-length=1? lst)
  (null? (cdr lst)))

(define (display-invalid-message message adt)
  (begin (display "Message: ")
         (display message)
         (display " not understood by ")
         (display adt)
         (newline)))

