#lang racket/base

(provide unsafe-vector-remove)

;; -----------------------------------------------------------------------------
;; =============================================================================

(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(< 0 n-1) (error 'unsafe-vector-remove "internal error")]
    [else
     (define new-vec (make-vector n-1 (vector-ref vec 0)))
     (let loop ([i 0])
       (when (i . < . k)
         (vector-set! new-vec i (vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([i k])
       (cond [(i . < . n-1)
              (vector-set! new-vec i (vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))
