#lang racket/base

(provide check-array-shape)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline))

;; =============================================================================

(begin-encourage-inline
  (define (check-array-shape ds fail)
    (define dims (vector-length ds))
    (define new-ds (make-vector dims 0))
    (let loop ([i 0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (cond [(<= 0 di)  (vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
)
