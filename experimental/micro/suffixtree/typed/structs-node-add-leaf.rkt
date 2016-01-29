#lang typed/racket/base

(provide node-add-leaf!)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         "data-node-adapted.rkt"
         benchmark-util)
(require/typed/check "structs-node-add-child.rkt"
  [node-add-child! (-> Node Node Void)])

;; =============================================================================

;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(: node-add-leaf! (-> Node Label Node))
(define (node-add-leaf! nd label)
  (let ([leaf (node label nd (list) #f)])
    (node-add-child! nd leaf)
    leaf))
