#lang racket/base

(provide node-add-leaf!)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
         "data-node.rkt"
(only-in "structs-node-add-child.rkt"
  node-add-child!))

;; =============================================================================

;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(define (node-add-leaf! nd label)
  (let ([leaf (node label nd (list) #f)])
    (node-add-child! nd leaf)
    leaf))
