#lang r5rs

(#%require "constants.rkt")
(#%provide (all-defined))






(define (pacman? adt)
  (equal? pacman-type (adt 'type)))

(define (coin? adt)
  (equal? coin-type (adt 'type)))

(define (edible? adt)
  (let ((type (adt 'type)))
    (or (equal? type coin-type))))