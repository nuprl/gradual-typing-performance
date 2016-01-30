#lang racket/base

(provide unsafe-vector-insert)

;; -----------------------------------------------------------------------------
;; =============================================================================

(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define dst-vec (make-vector (+ n 1) v))
  (let loop ([i 0])
    (when (i . < . k)
      (vector-set! dst-vec i (vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([i k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (vector-set! dst-vec i+1 (vector-ref vec i))
        (loop i+1))))
  dst-vec)
