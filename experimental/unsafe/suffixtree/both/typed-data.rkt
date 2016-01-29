#lang typed/racket/base

(provide
  Node Label Tree
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))

(define-type Node node)
(define-type Label label)
(define-type Tree suffix-tree)

(define-struct label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural]) #:mutable #:prefab)

;; A suffix tree consists of a root node.
(define-struct suffix-tree ([root : node]) #:prefab)

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(define-struct node ([up-label : label] [parent : (U #f node)] [children : (Listof node)] [suffix-link : (U #f node)]) #:mutable #:prefab)
