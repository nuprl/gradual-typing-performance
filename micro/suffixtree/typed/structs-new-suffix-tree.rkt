#lang typed/racket/base

(provide new-suffix-tree)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         "data-node-adapted.rkt"
         "data-suffix-tree-adapted.rkt"
         benchmark-util)
(require/typed/check "label-make-label.rkt"
  [make-label (-> (U String (Vectorof (U Char Symbol))) label)])
(require/typed/check "label-label-ref.rkt"
  [label-ref (-> label Integer (U Symbol Char))])

;; =============================================================================

;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(: new-suffix-tree (-> Tree))
(define (new-suffix-tree)
  (suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (node (make-label (make-vector 0 'X)) #f (list) #f)))
   ;; (let ((root (node (make-label (make-vector 0 'X)) #f (list) #f)))
   ;;   root)))
