#lang typed/racket

(provide
  Tree<%>
  ATree%
  Placed%
  LPlaced%
  tree-state
  lplaced%
  generate-tree
  tree-next
)
(require
  require-typed-check
  "../base/types.rkt"
  "board-adapted.rkt"
  "state-adapted.rkt"
)

;; -----------------------------------------------------------------------------

(define-type Tree<%>
  (Class
   (to-state (-> State))
   (next (-> Tile (Option Hotel) Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
   (founding (-> Natural (Listof (Listof Hotel)) Natural))
   ;; Precondition: share-order
   (traversal (-> Natural (Listof (Listof Hotel)) (-> (Instance Placed%) Natural) Natural))
   (lookup-tile (-> (-> (Listof Tile) Tile) (Listof HandOut) (Values (Option Tile) (Instance ATree%))))
   (merging (-> Natural (Listof (Listof Hotel)) Natural))))

(define-type ATree%
  (Class
   #:implements Tree<%>
   (init-field (state State))
   (nothing-to-place? (-> Boolean))))

;(define-type Tree (Instance ATree%))

(define-type Placed%
  (Class
   (init-field
    (state State)
    (tile Tile)
    (hotel (Option Hotel))
    (state/tile State)
    (reason SpotType))
   (purchase (-> Decisions (Listof Hotel) (U (Instance ATree%) (Listof HandOut))))
   ;; Precondition: share-order?
   (to-trees (-> Decisions (Listof Hotel) (Listof (Instance ATree%))))
   ;; Precondition: share-order?
   (acceptable-policies (-> (Listof (Listof Hotel)) (Listof (Listof Hotel))))))

(define-type LPlaced%
  (Class
   #:implements ATree%
   (init-field
    (lplaced (Listof (Instance Placed%)))
    (state State))
   ))


;; -----------------------------------------------------------------------------

(require/typed "tree.rkt"
  (#:opaque HandOut hand-out?)
  (lplaced% LPlaced%)
  (generate-tree (-> State (Instance ATree%)))
  (tree-next (-> (Instance ATree%) Tile Hotel Decisions (Listof Hotel) (-> (Listof Tile) Tile) (Values (Option Tile) (Instance ATree%))))
  (tree-state (-> (Instance ATree%) State))
)
