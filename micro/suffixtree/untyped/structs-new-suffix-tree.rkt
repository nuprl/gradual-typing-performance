#lang racket/base

(provide new-suffix-tree)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
         "data-node.rkt"
         "data-suffix-tree.rkt"
(only-in "label-make-label.rkt" make-label)
(only-in "label-label-ref.rkt" label-ref))

;; =============================================================================

;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(define (new-suffix-tree)
  (suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (node (make-label (make-vector 0 'X)) #f (list) #f)))
