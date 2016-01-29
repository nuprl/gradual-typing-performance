#lang racket

;; ---------------------------------------------------------------------------------------------------
;; an interface specification for an automated Acquire player module
;; see admin-intf for the actual player interface (player/c) 

(require "admin-intf.rkt" "basics.rkt" "board.rkt"  "strategy-intf.rkt" "Lib/contract.rkt")

(interface player&
  [player?  (-> any/c boolean?)]
  
  [create   (-> string? strategy/c (instanceof/c player/c))])

;; aadditional functions for testing
(interface player-extra&
  [test 
   ;; test a stragey on a specific board, the player's cash, the player's tiles, and available shares
   (-> strategy/c board? cash? (listof tile?) shares? (listof hotel?)
       ;; the tester: takes board, the player created from cash and tiles, and 
       ;; tests the player's choices of tiles and hotels to be acquired 
       (-> board? player/c (-> (maybe/c tile?) (maybe/c hotel?) shares-order/c any))
       any)]
  
  ;; --- externalize results of player's turn actions 
  [xaction? 
   (-> any/c boolean?)]
  
  [action->xexpr
   (-> (maybe/c tile?) (maybe/c hotel?) shares-order/c xaction?)])

