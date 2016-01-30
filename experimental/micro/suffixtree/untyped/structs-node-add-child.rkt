#lang racket/base

(provide node-add-child!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt")

;; =============================================================================

;; node-add-child!: node node -> void
;; Adds to the node list.
(define (node-add-child! node child)
  (set-node-children! node (cons child (node-children node))))
