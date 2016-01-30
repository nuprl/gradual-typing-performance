#lang racket

;; ---------------------------------------------------------------------------------------------------
;; an interface specification for an Acquire game strategy, assuming a
;; player that delegates all turn information at once and expects a
;; complete decision in return 

(require "basics.rkt" "board.rkt" "protocols.rkt" "Lib/auxiliaries.rkt" "Lib/contract.rkt")

(interface strategy&
  [ordered-s 
   ;; places the tile closest to A1 (in terms of tile<=?) that can be placed
   ;; and that buys as many shares as possible in alphabetical order 
   (make-strategy/c tile<?)]
  
  [random-s
   ;; places a random tile from the player's available tiles, buys a random number of random shares
   strategy/c]
  
  [largest-alpha 
   ;; 1. choose to place the LARGEST tile according to to tile<=? in the possession of the player
   ;; that can be placed on the current board, and
   ;; 2. choose to buy as many shares as possible in ALPHABETICAL order
   (make-strategy/c tile>?)]
  
  [smallest-anti
   ;; 1. choose to place the SMALLEST tile according to to tile<=? in the possession of the player 
   ;; that can be placed on the current board, and
   ;; 2. choose to buy as many shares as possible in ANTI-ALPHABETICAL order, but sort those on return
   (make-strategy/c tile<?)])

(provide strategy/c)

;; ---------------------------------------------------------------------------------------------------
;; auxiliary notions 

;; [Tile Tile -> Tile] -> Contract
;; a contract for strategies
;; compare placement to all tiles t in player-s-tiles that pass (cmp t tile)
(define (make-strategy/c cmp)
  ;; given: the current board, the player's tiles, the player's cash, 
  ;;        the remaining shares for sale, and the remaining hotel labels
  (->i ((turn (instanceof/c turn-player/c)))
       ;; compute: the tile to be placed (if the player can place one)
       ;;          the hotel involved in the placement 
       ;;          the shares to be bought and their price
       (values (tile (turn) (and/c (maybe/c tile?) (good-placement turn) (unique-tile turn cmp)))
               (hotel (tile turn) (and/c (maybe/c hotel?) (good-hotel-for-placement turn tile)))
               (buy-order (tile hotel turn) (and/c (listof hotel?) (good-shares turn tile hotel))))))

;; this is the minimal requirement for a strategy
(define strategy/c (make-strategy/c (lambda (s t) #f)))

;; ---------------------------------------------------------------------------------------------------
;; AUXILIARIES:

;; no tile closer to A1 than tile (in the sense of cmp) can be placed
;; examples: 
;; (check-true ((others-not-placable (board) (list (ctile A 1)) tile<?) (ctile A 1)))
;; (check-false ((others-not-placable (board) (list (ctile A 1)) tile<?) (ctile A 2)))
;; (check-true ((others-not-placable (board) (list (ctile A 1)) tile>?) (ctile A 1)))
;; (check-false ((others-not-placable (board) (list (ctile A 3)) tile>?) (ctile A 2)))
(define/contract (unique-tile turn cmp)
  (-> (instanceof/c turn-player/c) (-> tile? tile? any) (-> (maybe/c tile?) any))
  (lambda (tile)
    (define board (get-field board turn))
    (define player-s-tiles (get-field tiles turn))
    (or (boolean? tile)
        (and tile
             (for/and ((t (filter (lambda (t) (cmp t tile)) player-s-tiles)))
               (eq? (what-kind-of-spot board t) IMPOSSIBLE))))))
