#lang racket/base

(provide unsafe-array-index->value-index)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         (only-in racket/fixnum fx* fx+))

;; =============================================================================

(begin-encourage-inline
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([i 0] [j 0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (define ji (vector-ref js i))
             (loop (+ i 1) (fx+ ji (fx* di j)))]
            [else  j])))
)
