#lang typed/racket/base

(provide
 Board
 Tile
 tile?
 tile<=?
 tile->string
 ALL-TILES
 STARTER-TILES#
 FOUNDING GROWING MERGING SINGLETON IMPOSSIBLE
 *create-board-with-hotels
 affordable?
 board-tiles
 board?
 deduplicate/hotel
 distinct-and-properly-formed
 found-hotel
 free-spot?
 grow-hotel
 growing-which
 make-board
 merge-hotels
 merging-which
 place-tile
 set-board
 size-of-hotel
 what-kind-of-spot
 )

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 "../base/types.rkt")

(define-type Board (HashTable Tile Content))
(define-type Tile tile)
(define board? hash?)
(require/typed/check "board.rkt"
  (#:struct tile ([column : Column] [row : Row]))
  (tile<=? (-> Tile Tile Boolean))
  (tile->string (-> Tile String))
  (ALL-TILES (Listof Tile))
  (STARTER-TILES# Natural)
  (FOUNDING 'FOUNDING)
  (GROWING 'GROWING)
  (MERGING 'MERGING)
  (SINGLETON 'SINGLETON)
  (IMPOSSIBLE 'IMPOSSIBLE)
  (deduplicate/hotel (-> (Listof Hotel) (Listof Hotel)))
  (make-board (-> Board))
  (board-tiles (-> Board (Listof Tile)))
  (what-kind-of-spot (-> Board Tile SpotType))
  (growing-which (-> Board Tile (Option Hotel)))
  (merging-which (-> Board Tile (Values (Pairof Hotel (Listof Hotel)) (Listof Hotel))))
  (size-of-hotel (-> Board Hotel Natural))
  (free-spot? (-> Board Tile Boolean))
  (merge-hotels (-> Board Tile Hotel Board))
  (found-hotel (-> Board Tile Hotel Board))
  (grow-hotel (-> Board Tile Board))
  (place-tile (-> Board Tile Board))
  (set-board (-> Board Tile Kind (Option Hotel) Board))
  (affordable? (-> Board (Listof Hotel) Cash Boolean))
  (*create-board-with-hotels (-> (Listof Tile) (Listof (Pairof Hotel (Listof Tile))) Board))
  (distinct-and-properly-formed (-> (Listof Tile) (-> (Listof (Pairof Hotel (Listof Tile))) Boolean)))
)
