#lang typed/racket/base

(provide node-root?)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

(: node-root? (-> Node Boolean))
(define (node-root? node)
  (eq? #f (node-parent node)))
