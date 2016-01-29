#lang racket/base

(provide label-empty?)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(define (label-empty? label)
  (>= (label-i label) (label-j label)))
