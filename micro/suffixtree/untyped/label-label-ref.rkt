#lang racket/base

(provide label-ref)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

; label-ref: label number? -> char
; Returns the kth element in the label.
(define (label-ref label k)
  (unless (integer? k) (error "label ref INDEX"))
  (vector-ref (label-datum label) (+ k (label-i label))))
