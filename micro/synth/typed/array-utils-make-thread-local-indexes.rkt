#lang typed/racket/base

(provide make-thread-local-indexes)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt")

;; =============================================================================

(: make-thread-local-indexes (Integer -> (-> Indexes)))
(define (make-thread-local-indexes dims)
  (let: ([val : (Thread-Cellof (U #f Indexes)) (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let: ([v : Indexes  (make-vector dims 0)])
                (thread-cell-set! val v)
                v)))))
