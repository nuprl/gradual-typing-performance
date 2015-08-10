#lang typed/racket/base

(provide unsafe-vector-insert)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt")

;; =============================================================================

(: unsafe-vector-insert (All (I) ((Vectorof I) Integer I -> (Vectorof I))))
(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define: dst-vec : (Vectorof I) (make-vector (+ n 1) v))
  (let loop ([#{i : Integer} 0])
    (when (i . < . k)
      (vector-set! dst-vec i (vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Integer} k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (vector-set! dst-vec i+1 (vector-ref vec i))
        (loop i+1))))
  dst-vec)
