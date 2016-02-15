#lang racket/base

(provide
  assert
)

;; =============================================================================

(define (assert v p)
  (unless (p v)
    (error 'assert "value ~a does not pass test ~a" v (object-name p)))
  v)
