#lang typed/racket/base

(provide tree-contains?)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         "data-suffix-tree-adapted.rkt"
         benchmark-util)
(require/typed/check "structs-node-follow-k.rkt"
  [node-follow/k (-> Node
                     Label
                     (-> Node Boolean)
                     (-> Node Index Boolean)
                     (-> Node Label Index Boolean)
                     (-> Node Index Label Index Boolean)
                     Boolean)])

;; =============================================================================

;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(: tree-contains? (-> Tree Label Boolean))
(define (tree-contains? tree label)
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))
