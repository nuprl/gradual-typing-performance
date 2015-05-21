#lang typed/racket/base

(provide label-same-source?)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; label-same-source?: label label -> boolean
(: label-same-source? (-> label label Boolean))
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))
