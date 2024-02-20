#lang r5rs

(#%provide make-filereader-adt)

(define (make-filereader-adt filename)
  (let* ((port (open-input-file filename))
         (content (read port)))
    (close-input-port port)

    (define coins-announcer 'coins)
    (define walls-announcer 'walls)
    (define pacman-announcer 'pacman)
    (define ghosts-announcer 'ghosts)

    (define (make-get-function key)
      (lambda ()
        (let ((result (assq key content)))
          (if result
              (cadr result)
              result))))

    (define get-coins-positions ((make-get-function coins-announcer)))
    (define get-wall-positions ((make-get-function walls-announcer)))
    (define get-pacman-position ((make-get-function pacman-announcer)))
    (define get-ghost-range ((make-get-function ghosts-announcer)))


    (define filereader-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-wall-positions) get-wall-positions)
          ((eq? message 'get-pacman-position) get-pacman-position)
          ((eq? message 'get-ghost-range) get-ghost-range)
          ((eq? message 'get-coins-positions) get-coins-positions))))
    filereader-dispatch))