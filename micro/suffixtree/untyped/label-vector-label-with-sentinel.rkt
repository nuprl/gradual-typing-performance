#lang racket/base

(provide vector->label/with-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-make-sentinel.rkt" make-sentinel)
(only-in "label-vector-label.rkt" vector->label))

;; =============================================================================

;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(define (vector->label/with-sentinel vector)
  (define N (vector-length vector))
  (define V (make-vector (add1 N) (make-sentinel)))
  (let loop ((i 0))
    (if (< i N)
        (begin (vector-set! V i (vector-ref vector i))
               (loop (add1 i)))
        (vector->label V))))
