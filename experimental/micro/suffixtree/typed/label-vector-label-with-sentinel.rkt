#lang typed/racket/base

(provide vector->label/with-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-make-sentinel.rkt"
  [make-sentinel (-> Symbol)])
(require/typed/check "label-vector-label.rkt"
  [vector->label (-> (Vectorof (U Char Symbol)) label)])

;; =============================================================================

;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(: vector->label/with-sentinel (-> (Vectorof Char) label))
(define (vector->label/with-sentinel vector)
  (: N Index)
  (define N (vector-length vector))
  (: V (Vectorof (U Char Symbol)))
  (define V (make-vector (add1 N) (make-sentinel)))
  (let loop ((i 0))
    (if (< i N)
        (begin (vector-set! V i (vector-ref vector i))
               (loop (add1 i)))
        (vector->label V))))
