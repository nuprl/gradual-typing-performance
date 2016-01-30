#lang typed/racket/base

(provide label->vector)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Symbol Char))])

;; =============================================================================

;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->vector (-> label (Vectorof (U Char Symbol))))
(define (label->vector label)
  (: N Integer)
  (define N (label-length label))
  (: buffer (Vectorof (U Char Symbol)))
  (define buffer (make-vector N 'X));;'X is a placeholder
    (let loop ((i 0))
      (if (and (< i N) (index? i))
          (begin
            (vector-set! buffer i (label-ref label i))
           (loop (add1 i)))
          (vector->immutable-vector buffer))))
