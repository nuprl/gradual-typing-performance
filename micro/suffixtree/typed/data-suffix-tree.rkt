#lang typed/racket/base

(provide (struct-out suffix-tree)
         tree?
         tree-root)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

;; A suffix tree consists of a root node.
(struct suffix-tree ([root : node]))

(define tree? suffix-tree?)

(define tree-root suffix-tree-root)
