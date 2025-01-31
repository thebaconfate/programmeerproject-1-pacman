#lang r5rs

(#%provide (all-defined))

(define game-width 27)
(define beta-correction 2)
(define true-game-width (+ game-width beta-correction))

(define game-height 30)
(define alpha-correction 2)
(define true-game-height (+ game-height alpha-correction))

(define pixels²-per-position 25)
(define window-width-px (* pixels²-per-position true-game-width))
(define window-height-px (* pixels²-per-position true-game-height))

(define cel-width-px (/ window-width-px true-game-width))
(define cel-height-px (/ window-height-px true-game-height))

(define coin-type 'coin)
(define pacman-type 'pacman)
(define wall-type 'wall)
(define fruit-type '())

(define coin-score-value 10)

(define (create-line-of-positions starting-position end-position)
  (define (create-line create-position f)
    (begin
      (if (> (f starting-position)(f end-position))
          (let ((tmp end-position))
            (set! end-position starting-position)
            (set! starting-position tmp)))
      (let loop ((result '())
                 (i (f starting-position)))
        (if (= i (f end-position))
            (cons end-position result)
            (loop (cons (create-position i) result)(+ i 1))))))
  (let ((x1 (car starting-position))
        (x2 (car end-position)))
    (if (= x1 x2)
        (create-line (lambda (y) (cons x1 y)) cdr)
        (create-line (lambda (x) (cons x (cdr end-position))) car))))

(define (list-find ==? lst)
  (cond
    ((null? lst) #f)
    ((==? (car lst)) (car lst))
    (else (list-find ==? (cdr lst)))))

(define (remove-duplicates ==? lst)
  (let loop ((res '())
             (rem lst))
    (cond
      ((null? rem)(reverse res))
      ((list-find (lambda (el)(==? el (car lst)))(loop res (cdr lst))))
      (else (loop (cons (car list) res) (cdr lst))))))


(define (cons-unique ==? head tail)
  (if (list-find (lambda (el)(==? el head)) tail)
      tail
      (cons head tail)))

(define (reduce f acc args)
  (if (null? args)
      acc
      (reduce f (f acc (car args)) (cdr args))))

(define (append-uniques ==? . lsts)
  (reduce (lambda (outer-acc outer-arg)
            (reduce (lambda (inner-acc inner-arg)
                      (cons-unique ==? inner-arg inner-acc))
                    outer-acc
                    outer-arg))
          '()
          lsts))

(define (same-position? position1 position2)
  (and (= (car position1)(car position2))
       (= (cdr position1)(cdr position2))))

(define (create-square-of-positions x1 x2 y1 y2)
  (let*  ((upper-left (cons x1 y1))
          (upper-right (cons x2 y1))
          (lower-left (cons x1 y2))
          (lower-right (cons x2 y2))
          (res1 (create-line-of-positions upper-left lower-left))
          (res2 (create-line-of-positions upper-right lower-right))
          (res3 (create-line-of-positions upper-left upper-right))
          (res4 (create-line-of-positions lower-left lower-right)))
    (display res1)
    (newline)
    (display res2)
    (newline)
    (display res3)
    (newline)
    (display res4)
    (newline)
    (append-uniques (lambda (position1 position2)
                      (and (= (car position1)(car position2))
                           (= (cdr position1)(cdr position2))))
                    res1 res2 res3 res4)))

(define pacman-starting-position (cons 15 23))
(define coins-positions-per-level (list
                                   (cons 1
                                         (append-uniques
                                          same-position?
                                          (create-line-of-positions (cons 7 8)(cons 22 8))
                                          (create-line-of-positions (cons 7 8)(cons 7 23))
                                          (create-line-of-positions (cons 22 8)(cons 22 23))
                                          (create-line-of-positions (cons 7 23)(cons 14 23))
                                          (create-line-of-positions (cons 16 23)(cons 22 23))
                                          ))))

