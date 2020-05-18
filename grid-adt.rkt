;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Dependencies                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Grid ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-grid-adt vct-width vct-height)
  (let ((matrix (make-vector vct-height)))
    (let loop ((counter 0))
      (if (< counter (vector-length matrix))
          (begin (vector-set! matrix counter (make-vector vct-width))
                 (loop (+ counter 1)))))

    (define (object? object)
      (not (eq? object 0)))
    
    (define (peek width height)
      (vector-ref (vector-ref matrix height) width))
    
    (define (write! width height x)
      (vector-set! (vector-ref matrix  height) width x))

    (define (map-matrix! f)
      (let ((n-x (- (vector-length (vector-ref matrix 0))1))
            (n-y (- (vector-length matrix) 1)))
        (let loop ((i-x 0)
                   (i-y 0))
          (if (< i-y n-y)
              (if (= i-x n-x)
                  (loop 0 (+ i-y 1))
                  (if (eq? (peek i-x i-y) 0)
                      (loop (+ i-x game-step) i-y)
                      (begin ((peek i-x i-y)f)
                             (loop (+ i-x 1) i-y))))))))
    
    (define (draw-matrix! draw-adt)
      (let ((n-x (- (vector-length (vector-ref matrix 0))1))
            (n-y (- (vector-length matrix) 1)))
        (let loop ((i-x 0)
                   (i-y 0))
          (if (< i-y n-y)
              (if (= i-x n-x)
                  (loop 0 (+ i-y 1))
                  (if (or (eq? (peek i-x i-y) 0)
                          (eq? (peek i-x i-y) 1))
                      (loop (+ i-x game-step) i-y)
                      (begin (((peek i-x i-y)'draw!)draw-adt)
                             (loop (+ i-x 1) i-y))))))))

    (define (count-objects)
      (let ((n-x (- (vector-length (vector-ref matrix 0))1))
            (n-y (- (vector-length matrix) 1)))
        (let loop ((objects 0)
                   (i-x 0)
                   (i-y 0))
          (if (< i-y n-y)
              (if (= i-x n-x)
                  (loop objects 0 (+ i-y 1))
                  (if (eq? (peek i-x i-y) 0)
                      (loop objects (+ i-x game-step) i-y)
                      (loop (+ objects 1)(+ i-x 1) i-y)))
              objects))))
    
    (define (dispatch-matrix m)
      (cond
        ((eq? m 'peek)peek)
        ((eq? m 'write!) write!)
        ((eq? m 'map-matrix!)map-matrix!)
        ((eq? m 'draw-matrix!)draw-matrix!)
        ((eq? m 'count-objects)count-objects)))
    dispatch-matrix))