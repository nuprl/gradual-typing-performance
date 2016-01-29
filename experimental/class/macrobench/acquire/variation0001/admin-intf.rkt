#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for the game administrator class
;; includes assumptions that the administrator makes about the player class 

(require "basics.rkt" "board.rkt" "protocols.rkt" "state-intf.rkt" "state.rkt" "strategy-intf.rkt" 
         "Lib/contract.rkt")

(interface admin&
  (administrator%
   ;; an Acquire game administrator 
   administrator/c)
  
  (turn% 
   ;; a proxy object that represents a player's powers during a turn 
   turn-player/c)
  
  ;; the administrator's return status 
  DONE EXHAUSTED SCORE)

(provide administrator/c player/c)

;; ---------------------------------------------------------------------------------------------------
;; auxiliary notions 

(define administrator/c 
  (class/c
   (init-field (next-tile (-> (listof tile?) tile?)))
   
   ;; signup a player and assign unique name to it 
   (sign-up (->m string? (instanceof/c (recursive-contract player/c)) string?))
   
   ;; produce the unique names of the players signed up so far
   (show-players (->m (listof string?)))
   
   ;; run a game of Acquire with the currently signed up players 
   ;; produce the final state and a score board 
   (run
    (->dm ((turns# natural-number/c)) (#:show (show (-> natural-number/c state? any)))
          ;; must replace later
          (values (status symbol?) 
                  (score any/c)
                  (los (and/c (listof state?) #; (lambda (los) (<= (length los) n)))))))))

(define player/c
  (class/c
   (init-field (name string?))
   
   ;; this information isn't needed for the interaction protocol:
   (init-field (choice strategy/c)) 
   
   (go 
    ;; go register yourself with the administrator%
    (->m (instanceof/c administrator/c) any))
   
   (setup
    ;; receive the game's complete state
    (->m state? any))
   
   (take-turn
    ;; it is this player's turn
    ;; given: the current board, the knowledge of the game admin about the player 
    ;;        the remaining shares for sale, and the remaining hotel labels
    ;; compute: the tile to be placed (if the player can place one)
    ;;          the hotel involved in the placement 
    ;;          the shares to be bought
    ;; [I'd like to be able to say formally, according to the choice, the given strategy
    (->dm ([turn (instanceof/c turn-player/c)])
          (values
           (tile (and/c (maybe/c tile?) (good-placement turn)))
           (hotel (and/c (maybe/c hotel?) (good-hotel-for-placement turn tile)))
           (to-buy (and/c shares-order/c (good-shares turn tile hotel))))))
   
   (keep
    ;; does this player wish to keep the shares of the acquired hotles
    (->dm ([acquired-hotels (listof hotel?)]) 
          (decisions (and/c (listof boolean?) (compose (=/c (length acquired-hotels)) length)))))
   
   (receive-tile
    ;; at the end of the player's turn it receives a tile, if one is available 
    (->m tile? any))
   
   (inform 
    ;; accept a complete new state at the end of a turn 
    (->m state? any))
   
   (the-end
    ;; the results as one fat string
    (->m state? score/c any))))

