#lang typed/racket/base

(provide array-shape-size)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt")

;; =============================================================================

(begin-encourage-inline
  (: array-shape-size (Indexes -> Integer))
  (define (array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([#{i : Integer} 0] [#{n : Integer} 1])
      (cond [(i . < . dims)  (define d (vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
)
