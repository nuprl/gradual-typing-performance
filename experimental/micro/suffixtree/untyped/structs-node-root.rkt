#lang racket/base

(provide node-root?)

;; -----------------------------------------------------------------------------

(require "data-node.rkt")

;; =============================================================================

(define (node-root? node)
  (eq? #f (node-parent node)))
