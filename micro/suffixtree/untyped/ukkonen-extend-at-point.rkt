#lang racket/base

(provide extend-at-point!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-sublabel.rkt"
  sublabel)
(only-in "structs-node-position-at-end.rkt"
  node-position-at-end?)
(only-in "structs-node-add-leaf.rkt"
  node-add-leaf!)
(only-in "structs-node-up-splice-leaf.rkt"
  node-up-splice-leaf!))

;; =============================================================================

;; extend-at-point!: node number label number -> node
(define (extend-at-point! node offset label i)
  (define (main-logic node offset label i)
    (if (should-extend-as-leaf? node offset)
        (attach-as-leaf! node label i)
        (splice-with-internal-node! node offset label i)))
  (define (should-extend-as-leaf? node offset)
    (node-position-at-end? node offset))
  (define (attach-as-leaf! node label i)
    (define leaf (node-add-leaf! node (sublabel label i)))
    node)
  (define (splice-with-internal-node! node offset label i)
    ;; otherwise, extend by splicing
    (define-values (split-node leaf)
      (node-up-splice-leaf!
       node offset (sublabel label i)))
    split-node)
  (main-logic node offset label i))
