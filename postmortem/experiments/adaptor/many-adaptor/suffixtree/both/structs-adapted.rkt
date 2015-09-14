#lang typed/racket/base

(require "label-adapted.rkt" benchmark-util)

(require/typed/check "structs.rkt"
  [#:struct node ([up-label : Label]
                  [parent : (U #f Node)]
                  [children : (Listof Node)]
                  [suffix-link : (U #f Node)])]
  [make-suffix-tree (-> Node Tree)]
  [make-tree (-> Tree)]
  [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
  [set-node-children! (-> Node (Listof Node) Void)]
  [set-node-up-label! (-> Node Label Void)]
  [set-node-parent! (-> Node Node Void)]
  [set-node-suffix-link! (-> Node Node Void)]
  [new-suffix-tree (-> Tree)]
  [node-find-child (-> Node Any (U Node #f))]
  [node-root? (-> Node Boolean)]
  [node-position-at-end? (-> Node Index Boolean)]
  [node-add-leaf! (-> Node Label Node)]
  [node-up-splice-leaf! (-> Node Index Label (values Node Node))]
  [node-follow/k (-> Node
                     Label
                     (-> Node (Pairof Node Index))
                     (-> Node Index (Pairof Node Index))
                     (-> Node Label Index (Pairof Node Index))
                     (-> Node Index Label Index (Pairof Node Index))
                     (Pairof Node Index))]
  [#:struct suffix-tree ([root : Node])])

(define-type Tree suffix-tree)
(define-type Node node)

(provide
  Tree Node
  node-root? node-position-at-end? node-add-leaf! node-up-splice-leaf! node-follow/k
  node-find-child
  new-suffix-tree
  node node? node-up-label node-parent node-children node-suffix-link
  suffix-tree? suffix-tree-root
  make-suffix-tree
  make-node
  make-tree
  (rename-out [suffix-tree-root tree-root])
  set-node-children! set-node-up-label! set-node-parent! set-node-suffix-link!
)

