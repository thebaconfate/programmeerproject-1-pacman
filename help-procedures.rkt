#lang r5rs

(#%provide (all-defined))

(define (pacman? adt)
  (equal? 'pacman (adt 'type)))

(define (ghost? adt)
  (equal? 'ghost (adt 'type)))

(define (fruit? adt)
  (equal? 'fruit (adt 'type)))

(define (abs x)
  (if (< x 0) (- x) x))