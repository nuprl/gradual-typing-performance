#lang typed/racket/base

(provide label-length)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; label-length: label -> number?
;; Returns the length of the label.
(: label-length (-> label Index))
(define (label-length label)
  (define len (- (label-j label) (label-i label)))
  (unless (index? len) (error "label-length"))
  len)
