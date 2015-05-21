#lang racket/base

(provide label->vector)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-length.rkt" label-length)
(only-in "label-label-ref.rkt" label-ref))

;; =============================================================================

;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.
(define (label->vector label)
  (define N (label-length label))
  (define buffer (make-vector N 'X));;'X is a placeholder
    (let loop ((i 0))
      (if (and (< i N) (integer? i))
          (begin
            (vector-set! buffer i (label-ref label i))
           (loop (add1 i)))
          (vector->immutable-vector buffer))))
