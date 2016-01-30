#lang racket 

;; ---------------------------------------------------------------------------------------------------
;; interface specification for an Acquire game tree: creating, navigating, and inspecting 

(require "basics.rkt" "board.rkt" "state.rkt" "Lib/contract.rkt")

(interface tree&
  (tree? (-> any/c boolean?))
  
  (decision-tree?
   ;; is this a tree where decisions can be made? 
   (-> tree? boolean?))

  (generate-tree
   ;; generate a game tree starting from this state
   (-> state? tree?))
  
  (tree-state 
   ;; all branches in a tree must start from the same state 
   (-> tree? state?))
  
  (tree-next
   ;; given a tile placement, optional hotel, and shares to buy plus a way to select the hand out tile
   ;; navigate to the next subtree of this decision tree,
   (-> decision-tree?
       tile?
       (maybe/c hotel?)
       ;; we should check that the hotels in the following list are acquired in this move
       (listof (list/c player? (listof (list/c hotel? boolean?))))
       shares-order/c
       ;; NOTE: an alternative to a choice function is to fix the list of tile order once and for all
       (->i ([lot (listof tile?)]) (one-of-them (lot) (and/c tile? (lambda (x) (member x lot)))))
       ;; -- yields -- 
       (values tile? tree?)))
  
  (tree-founding
   ;; how many founding transitions are three in ct up to depth n
   (-> tree? natural-number/c policies/c natural-number/c))
  
  (tree-merging
   ;; how many merging transitions are three in ct up to depth n
   (-> tree? natural-number/c policies/c natural-number/c)))

;; ---------------------------------------------------------------------------------------------------
;; auxiliary notions 

;; I'd like to do this but I can't yet: 
;; (define query/c (-> tree? natural-number/c policies natural-number/c))

(define policies/c (listof shares-order/c))
