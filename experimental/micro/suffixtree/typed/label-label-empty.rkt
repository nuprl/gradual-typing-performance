#lang typed/racket/base

(provide label-empty?)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(: label-empty? (-> label Boolean))
(define (label-empty? label)
  (>= (label-i label) (label-j label)))
