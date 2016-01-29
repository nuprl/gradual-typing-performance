#lang racket/base

(provide label-length)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; label-length: label -> number?
;; Returns the length of the label.
(define (label-length label)
  (define len (- (label-j label) (label-i label)))
  (unless (integer? len) (error "label-length"))
  len)
