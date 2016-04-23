#lang typed/racket/base

(require benchmark-util)

(require/typed/check "data.rkt"
  [label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
  [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
  (label? (-> Any Boolean))
  (label-i (-> Label Natural))
  (label-j (-> Label Natural))
  (label-datum (-> Label (Vectorof (U Char Symbol))))
  [set-label-i! (-> Label Natural Void)]
  [set-label-j! (-> Label Natural Void)]
  [node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
  (node? (-> Any Boolean))
  [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
  (node-up-label (-> Node Label))
  (node-parent (-> Node (U #f Node)))
  (node-children (-> Node (Listof Node)))
  (node-suffix-link (-> Node (U #f Node)))
  [set-node-children! (-> Node (Listof Node) Void)]
  [set-node-up-label! (-> Node Label Void)]
  [set-node-parent! (-> Node Node Void)]
  [set-node-suffix-link! (-> Node Node Void)]
  [suffix-tree (-> Node Tree)]
  (suffix-tree? (-> Any Boolean))
  [make-suffix-tree (-> Node Tree)]
  (suffix-tree-root (-> Tree Node))
)

(define-type Label (Vector 'label (Vectorof (U Char Symbol)) Natural Natural))
(define-type Node (Vector 'node Label (U #f Node) (Listof Node) (U #f Node)))
(define-type Suffix-Tree (Pairof 'suffix-tree (List Node)))
(define-type Tree Suffix-Tree)

(provide
  Label Node Tree Suffix-Tree
  label
  make-label
  label?
  label-i
  label-j
  label-datum
  set-label-i!
  set-label-j!
  node
  node?
  make-node
  node-up-label
  node-parent
  node-children
  node-suffix-link
  set-node-children!
  set-node-up-label!
  set-node-parent!
  set-node-suffix-link!
  suffix-tree
  suffix-tree?
  make-suffix-tree
  suffix-tree-root
)
