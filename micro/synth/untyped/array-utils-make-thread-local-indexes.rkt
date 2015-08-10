#lang racket/base

(provide make-thread-local-indexes)

;; -----------------------------------------------------------------------------

;; =============================================================================

(define (make-thread-local-indexes dims)
  (let ([val (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let ([v  (make-vector dims 0)])
                (thread-cell-set! val v)
                v)))))
