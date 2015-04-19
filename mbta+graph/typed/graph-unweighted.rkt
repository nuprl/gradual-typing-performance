#lang typed/racket/base

(require
 benchmark-util
 "graph-types.rkt"
 (only-in racket/set in-set))

(require/typed/check "graph-adjlist-utils.rkt"
  [add-edge@   (-> (HashTable String (Setof String)) String String Void)]
  [add-vertex@ (-> (HashTable String (Setof String)) String Void)])
 
; unweighted, adjacency-list graph

(provide
 unweighted-graph/directed)

;; directed graph constructor
(: unweighted-graph/directed (-> (Listof (List String String)) unweighted-graph))
(define (unweighted-graph/directed es)
  ;; Set up edges
  (: adj (HashTable String (Setof String)))
  (define adj (make-hash))
  (for ([e : (List String String) es]) 
    (apply add-edge@ adj e)
    (add-vertex@ adj (cadr e)))
  ;; Define graph functions
  (: get-vertices (-> (Listof String)))
  (define (get-vertices) (hash-keys adj))
  (: in-neighbors (-> String (Sequenceof String)))
  (define (in-neighbors v)
    (in-set 
     (hash-ref adj v 
               (λ () (error 'in-vertices "vertex not in graph")))))
  ;; returns edges as a sequence
  ;; Create graph
  (unweighted-graph
   get-vertices
   in-neighbors
))
