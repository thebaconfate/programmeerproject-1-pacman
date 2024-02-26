#lang r5rs

(#%require "constants.rkt")
(#%provide (all-defined))

(define (adt-type adt)
  (adt 'get-type))

(define (pacman? adt)
  (equal? pacman-type (adt 'get-type)))

(define (coin? adt)
  (equal? coin-type (adt 'get-type)))

(define (edible? adt)
  (and (procedure? adt)
       (or (equal? coin-type (adt-type adt)))))

(define (wall? adt)
  (and (procedure? adt)(equal? wall-type (adt 'get-type))))

(define level-1-coin-list-of-row-y   '(19 21 23))
(define level-1-coin-list-of-start-x '( 8  8  8))
(define level-1-coin-list-of-end-x   '(19 19 19))
;;Column values:
(define level-1-coin-list-of-column-x  '( 8 19))
(define level-1-coin-list-of-start-y   '(19 19))
(define level-1-coin-list-of-end-y     '(23 23))

(define (create-list-of-positions starting-x ending-x starting-y ending-y)
  (let loop ((x starting-x)
             (y starting-y)
             (acc '()))
    (if (> x ending-x)
        acc
        (if (> y ending-y)
            (loop (+ x 1) starting-y acc)
            (loop x (+ y 1) (cons (list x y) acc))))))

(define coins-locations (remove-dups (append
                                      (create-list-of-positions 8 19 19 19)
                                      (create-list-of-positions 8 19 21 21)
                                      (create-list-of-positions 8 19 19 23)
                                      (create-list-of-positions 8 8 19 23)
                                      (create-list-of-positions 19 19 19 23))))
(define (string-join list-of-strings char)
  (let loop ((result "")
             (list-of-strings list-of-strings))
    (if (null? list-of-strings)
        result
        (loop (string-append result (car list-of-strings) char) (cdr list-of-strings)))))

(define (remove item lst)
  (if (null? lst)
      '()
      (if (equal? item (car lst))
          (cdr lst)
          (cons (car lst) (remove item (cdr lst))))))