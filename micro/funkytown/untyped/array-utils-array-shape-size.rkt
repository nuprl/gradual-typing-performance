#lang racket/base

(provide array-shape-size)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline))

;; =============================================================================

(begin-encourage-inline
  (define (array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([i 0] [n 1])
      (cond [(i . < . dims)  (define d (vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
)
