#lang typed/racket/base

(provide Label Tree Node)


(require/typed/provide "data.rkt"
  [#:struct label ([datum : (Vectorof (U Char Symbol))]
                   [i : Natural] [j : Natural])]
  [make-label (-> (Vectorof (U Char Symbol)) Natural Natural Label)]
  [set-label-i! (-> Label Natural Void)]
  [set-label-j! (-> Label Natural Void)]
  [#:struct node ([up-label : Label]
                  [parent : (U #f Node)]
                  [children : (Listof Node)]
                  [suffix-link : (U #f Node)])]
  [make-suffix-tree (-> Node Tree)]
  [make-node (-> Label (U #f Node) (Listof Node) (U #f Node) Node)]
  [set-node-children! (-> Node (Listof Node) Void)]
  [set-node-up-label! (-> Node Label Void)]
  [set-node-parent! (-> Node Node Void)]
  [set-node-suffix-link! (-> Node Node Void)]
  [#:struct suffix-tree ([root : Node])])

(define-type Label label)
(define-type Tree suffix-tree)
(define-type Node node)

