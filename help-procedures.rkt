;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Help Procedures                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (direction? key)
  (or (eq? key 'up)
      (eq? key 'right)
      (eq? key 'down)
      (eq? key 'left)))

(define (wall? x)
  (eq? x 'wall))

(define (gate? x)
  (eq? x 'gate))

(define (fruit? x)
    (or (eq? x 'fruit)
        (eq? x 'cherry)
        (eq? x 'strawberry)
        (eq? x 'orange)
        (eq? x 'apple)
        (eq? x 'melon)))

(define (coin? x)
  (eq? x 'coin))

(define (pacman? x)
  (eq? x 'pacman))

(define (ghost? x)
  (or
   (eq? x 'ghost)
   (eq? x 'blue-ghost)
   (eq? x 'orange-ghost)
   (eq? x 'pink-ghost)
   (eq? x 'red-ghost)))

(define (scared-ghost? x)
  (eq? x 'scared-ghost))

(define (powerup? x)
  (eq? x 'powerup))

(define (half x)
  (ceiling (/ x 2)))

(define (3fourths x)
  (ceiling(*(/ x 4)3)))

(define (count-elements arg)
  (let loop ((lst arg)
             (counter 0))
    (if (null? lst)
        counter
        (loop (cdr lst) (+ counter 1)))))
