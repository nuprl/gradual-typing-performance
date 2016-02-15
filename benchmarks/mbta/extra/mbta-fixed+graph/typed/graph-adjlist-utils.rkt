#lang typed/racket/base

(require
 racket/set)

(provide
 add-edge@
 add-vertex@
)

;; An AdjacencyList is a [MutableHashOf String -> [Setof String]]
;;   and is used as the internal graph representation

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)

;; (internal graph functions and names have a @ suffix)
(: add-edge@ (-> (HashTable String (Setof String)) String String Void))
(define (add-edge@ adj u v)
  (: new-set (Setof String))
  (define new-set (set))
  (hash-update! adj u (λ ([vs : (Setof String)]) (set-add vs v)) (lambda () new-set)))

(: add-vertex@ (-> (HashTable String (Setof String)) String Void))
(define (add-vertex@ adj v)
  (: new-set (Setof String))
  (define new-set (set))
  (hash-update! adj v (λ ([vs : (Setof String)]) vs) (lambda () new-set)))
