#lang racket/base

(require
 "graph-struct.rkt"
 "graph-adjlist-utils.rkt"
 (only-in racket/set in-set))

; unweighted, adjacency-list graph

(provide
 unweighted-graph/directed
 (struct-out unweighted-graph))

;; directed graph constructor
(define (unweighted-graph/directed es)
  ;; Set up edges
  (define adj (make-hash))
  (for ([e es]) 
    (cond [(list? e) (apply add-edge@ adj e)
                     (add-vertex@ adj (cadr e))]
          [else (add-vertex@ adj e)]))
  ;; Define graph functions
  (define (get-vertices) (hash-keys adj))
  (define (in-neighbors v)
    (in-set 
     (hash-ref adj v 
               (λ () (error 'in-vertices "vertex not in graph")))))
  ;; returns edges as a sequence
  ;; Create graph
  (unweighted-graph
   get-vertices
   in-neighbors))
