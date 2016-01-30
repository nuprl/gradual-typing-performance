#lang racket/base

(provide label-ref-at-end?)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-length.rkt" label-length))

;; =============================================================================

;; label-ref-at-end?: label number -> boolean
(define (label-ref-at-end? label offset)
  (= offset (label-length label)))
