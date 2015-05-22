#lang typed/racket/base

(provide node-add-child!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

;; node-add-child!: node node -> void
;; Adds to the node list.
(: node-add-child! (-> Node Node Void))
(define (node-add-child! node child)
  (set-node-children! node (cons child (node-children node))))
