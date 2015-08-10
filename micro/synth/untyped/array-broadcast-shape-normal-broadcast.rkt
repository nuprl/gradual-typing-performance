#lang racket/base

(provide shape-normal-broadcast)

;; -----------------------------------------------------------------------------

;; =============================================================================

(define (shape-normal-broadcast ds1 ds2 dims fail)
  (define new-ds (make-vector dims 0))
  (let loop ([k 0])
    (cond [(k . < . dims)
           (define dk1 (vector-ref ds1 k))
           (define dk2 (vector-ref ds2 k))
           (vector-set!
            new-ds k
            (cond [(= dk1 dk2)  dk1]
                  [(and (= dk1 1) (dk2 . > . 0))  dk2]
                  [(and (= dk2 1) (dk1 . > . 0))  dk1]
                  [else  (fail)]))
           (loop (+ k 1))]
          [else  new-ds])))
