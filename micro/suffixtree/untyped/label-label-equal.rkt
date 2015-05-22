#lang racket/base

(provide label-equal?)

;; -----------------------------------------------------------------------------

(require
         "data-label.rkt"
(only-in "label-label-length.rkt" label-length)
(only-in "label-label-prefix.rkt" label-prefix?))


;; =============================================================================

;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(define (label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))
