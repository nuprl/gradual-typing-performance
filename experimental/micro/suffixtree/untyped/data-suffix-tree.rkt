#lang racket/base

(provide (struct-out suffix-tree)
         tree?
         tree-root)

;; -----------------------------------------------------------------------------

(require "data-node.rkt")

;; =============================================================================

;; A suffix tree consists of a root node.
(struct suffix-tree (root))

(define tree? suffix-tree?)

(define tree-root suffix-tree-root)
