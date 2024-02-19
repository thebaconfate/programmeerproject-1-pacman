#lang r5rs

(define (make-filereader-adt filename)
  (let* ((port (open-input-file filename))
         (content (read port)))
    (close-input-port port)

    (define coins-announcer 'coins)
    (define walls-announcer 'walls)
    (define pacman-announcer 'pacman)
    (define ghosts-announcer 'ghosts)

    (define get-coins-positions (assq coins-announcer content))
    (define get-wall-positions (assq walls-announcer content))
    (define get-pacman-position (assq pacman-announcer content))
    (define get-ghost-range (assq ghosts-announcer content))


    (define filereader-dispatch
      (lambda (message)
        (cond
          ((eq? message 'get-coins-positions) get-coins-positions)
          ((eq? message 'content) content))))
    filereader-dispatch))
