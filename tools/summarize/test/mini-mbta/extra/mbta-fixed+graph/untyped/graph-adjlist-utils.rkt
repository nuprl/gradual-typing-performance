#lang racket/base

(require racket/set)

(provide
 add-edge@
 add-vertex@
)

;; An AdjacencyList is a [MutableHashOf Vertex -> [Setof Vertex]]
;;   and is used as the internal graph representation

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)

;; (internal graph functions and names have a @ suffix)
(define (add-edge@ adj u v) (hash-update! adj u (λ (vs) (set-add vs v)) (set)))
(define (add-vertex@ adj v) (hash-update! adj v (λ (vs) vs) (set)))
