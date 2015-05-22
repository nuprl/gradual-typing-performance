#lang typed/racket/base

(provide label-ref)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

; label-ref: label number? -> char
; Returns the kth element in the label.
(: label-ref (-> label Integer (U Symbol Char)))
(define (label-ref label k)
  (unless (index? k) (error "label ref INDEX"))
  (vector-ref (label-datum label) (+ k (label-i label))))
