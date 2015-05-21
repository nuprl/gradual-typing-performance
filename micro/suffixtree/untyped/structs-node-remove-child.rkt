#lang racket/base

(provide node-remove-child!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt")

;; =============================================================================

;; node-remove-child!: node node -> void
;; Removes child node.
(define (node-remove-child! node child)
  (set-node-children! node (remq child (node-children node))))
