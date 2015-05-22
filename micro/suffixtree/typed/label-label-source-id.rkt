#lang typed/racket/base

(provide label-source-id)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; label-source-id: label -> number
(: label-source-id (-> label Integer))
(define (label-source-id label)
  (eq-hash-code (label-datum label)))
