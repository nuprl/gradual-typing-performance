#lang racket/base

(provide label-same-source?)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; label-same-source?: label label -> boolean
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))
