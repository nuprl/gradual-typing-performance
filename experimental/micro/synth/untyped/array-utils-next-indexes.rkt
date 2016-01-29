#lang racket/base

(provide next-indexes!)

;; -----------------------------------------------------------------------------

;; =============================================================================

;; Sets js to the next vector of indexes, in row-major order
(define (next-indexes! ds dims js)
  (let loop ([k  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (vector-ref js k))
        (define dk (vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (vector-set! js k 0)
                 (loop k)]
                [else
                 (vector-set! js k jk)]))))))
