#lang typed/racket/base

(provide label-equal?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-label-adapted.rkt")
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "label-label-prefix.rkt"
  [label-prefix? (-> label label Boolean)])


;; =============================================================================

;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(: label-equal? (-> label label Boolean))
(define (label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))
