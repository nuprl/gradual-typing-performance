#lang typed/racket/base

(provide unsafe-vector-remove)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt")

;; =============================================================================

(: unsafe-vector-remove (All (I) ((Vectorof I) Integer -> (Vectorof I))))
(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define: new-vec : (Vectorof I) (make-vector n-1 (vector-ref vec 0)))
     (let loop ([#{i : Integer} 0])
       (when (i . < . k)
         (vector-set! new-vec i (vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([#{i : Integer} k])
       (cond [(i . < . n-1)
              (vector-set! new-vec i (vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))
