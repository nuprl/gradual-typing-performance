#lang racket/base

(provide label-source-id)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; label-source-id: label -> number
(define (label-source-id label)
  (eq-hash-code (label-datum label)))
