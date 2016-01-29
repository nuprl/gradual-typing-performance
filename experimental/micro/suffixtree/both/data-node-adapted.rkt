#lang typed/racket/base

(provide (struct-out node)
         set-node-children!
         set-node-up-label!
         set-node-parent!
         set-node-suffix-link!
         Node)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "data-node.rkt"
  [#:struct node ([up-label : label]
                  [parent : (U #f node)]
                  [children : (Listof node)]
                  [suffix-link : (U #f node)])]
  [set-node-children! (-> Node (Listof Node) Void)]
  [set-node-parent! (-> Node Node Void)]
  [set-node-suffix-link! (-> Node Node Void)]
  [set-node-up-label! (-> Node label Void)])

;; =============================================================================

(define-type Node node)
