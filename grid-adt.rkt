#lang r5rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 grid ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%provide make-grid-adt)

;; Author: Gérard Lichtert

;; WARNING: this class only works with positive integers.
;; You can create a grid with (make-grid-adt integer integer) -> grid-adt
;; you can read the grid at index i j with ((grid-adt 'read-grid) integer integer) -> any
;; you can write a new-value to the grid at i j with ((grid-adt 'write-grid!) integer integer any) -> /
;; you can apply a procedure to each index of the grid with ((grid-adt 'for-each-grid) procedure) -> /
;; you can map a procedure to each element of the grid with ((grid-adt 'map-grid) procedure) -> grid-adt
;; you can convert the grid to a sting with (grid-adt 'to-string) -> string

(define (make-grid-adt dim-i dim-j)
  (let ((grid (make-vector dim-i)))
    (let loop ((i 0))
      (if (< i dim-i)
          (begin
            (vector-set! grid i (make-vector dim-j))
            (loop (+ i 1)))))

    (define grid? #t)

    (define (primitive? value)
      (or (number? value)
          (boolean? value)
          (symbol? value)
          (string? value)))

    (define read-grid
      (lambda (i j)
        (vector-ref (vector-ref grid i) j)))

    (define write-grid!
      (lambda (i j new-value)
        (vector-set! (vector-ref grid i) j new-value)))

    (define (for-each-grid proc)
      (let loop ((i 0)
                 (j 0))
        (if (< i dim-i)
            (if (< j dim-j)
                (begin
                  (proc (read-grid i j))
                  (loop i (+ j 1)))
                (loop (+ i 1) 0)))))

    (define (map-grid proc)
      (let ((new-grid (make-grid-adt dim-i dim-j)))
        (let loop ((i 0)
                   (j 0))
          (if (< i dim-i)
              (if (< j dim-j)
                  (begin
                    ((new-grid 'write-grid!) i j (proc (read-grid i j)))
                    (loop i (+ j 1)))
                  (loop (+ i 1) 0))
              new-grid))))

    (define to-string
      (lambda ()
        (define cols
          (let loop ((j 0))
            (if (< j dim-j)
                (string-append (number->string j) " " (loop (+ j 1)))
                "\n")))
        (define bound->string
          (let loop ((counter 0)
                     (limit (+ dim-j 2)))
            (if (< counter limit)
                (string-append "- " (loop (+ 1 counter) limit))
                "\n")))
        (define (primitive->string primitive)
          (define (boolean->string boolean)
            (if (eq? #t boolean)
                "#t"
                "#f"))
          (cond
            ((char? primitive)primitive)
            ((string? primitive)primitive)
            ((symbol? primitive)(symbol->string primitive))
            ((number? primitive)(number->string primitive))
            ((boolean? primitive)(boolean->string primitive))
            (else "ERROR: Primitive not handled in conditional")))
        (define (string->char string)
          (string-ref string 0))
        (define (line->string i)
          (string-append (number->string i)
                         (string-append
                          "| "
                          (let loop ((j 0))
                            (if (< j dim-j)
                                (let ((peek-value (read-grid i j)))
                                  (if (primitive? peek-value)
                                      (string-append
                                       (string-append
                                        (primitive->string peek-value) " ")
                                       (loop (+ j 1)))
                                      (string-append
                                       (string-append
                                        (string (string->char
                                                 (peek-value 'to-string))) " ")
                                       (loop (+ j 1)))))
                                "|\n")))))
        (string-append cols (string-append bound->string
                                           (let loop ((i 0))
                                             (if (< i dim-i)
                                                 (string-append (line->string i)(loop (+ i 1)))
                                                 bound->string))))))

    (define grid-dispatch
      (lambda (message)
        (cond
          ((eq? message 'grid?)grid?)
          ((eq? message 'read-grid)read-grid)
          ((eq? message 'write-grid!)write-grid!)
          ((eq? message 'for-each-grid)for-each-grid)
          ((eq? message 'map-grid)map-grid)
          ((eq? message 'to-string)(to-string))
          (else "ERROR: Method not recognized by GRID ADT"))))
    grid-dispatch))


