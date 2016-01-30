#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for basic Acquire pieces and concepts:
;; -- hotels
;; -- cash
;; -- shares 
;; -- plus functions that encode Acquire rules at this level

(require "Lib/auxiliaries.rkt" "Lib/contract.rkt")

(interface basics&
  [MIN-PLAYER# natural-number/c]
  [MAX-PLAYER# natural-number/c]

  [hotel?       (-> any/c boolean?)]
  [hotel<=?     (-> hotel? hotel? boolean?)]
  [random-hotel (-> hotel?)]
  [AMERICAN     hotel?]
  [CONTINENTAL  hotel?]
  [FESTIVAL     hotel?]
  [IMPERIAL     hotel?]
  [SACKSON      hotel?]
  [TOWER        hotel?]
  [WORLDWIDE    hotel?]  
  [ALL-HOTELS   (and/c (listof hotel?) (sorted hotel<=?))]
  [SAFE#        natural-number/c] ;; a hotel that consists of at least SAFE# tiles is safe
  [FINAL#       natural-number/c] ;; a hotel that consists of at least FINAL# tiles ends the game 
  ;; externalizing 
  [string->hotel (-> string? (maybe/c hotel?))]
  [hotel->label  (-> hotel? string?)]
  [hotel->color  (-> hotel? symbol?)]
  [string->count (-> string? (maybe/c natural-number/c))]
  [xhotel?       (-> any/c boolean?)]
  [hotel->xexpr  (-> hotel? xhotel?)]

  ;; wallets of shares 
  [shares?            (-> any/c boolean?)]
  [banker-shares0     shares?]
  [player-shares0     shares?]
  [shares-order/c     contract?]
  [SHARES-PER-TURN#   natural-number/c]
  [shares++ (-> shares? hotel? shares?)]
  [shares-- (->i ((s shares?) (h hotel?)) #:pre (s h) (> (shares-available s h) 0) (r shares?))]
  [shares-available
    ;; how many shares are available for the given hotel in this wallet? 
    (-> shares? hotel? natural-number/c)]
  [shares-available?
    ;; can the given order of shares be satisfied in this wallet? 
    (-> shares? shares-order/c boolean?)]
  [shares-minus       (-> shares? shares? shares?)]
  [shares-plus        (-> shares? shares? shares?)]
  [shares-compatible  (-> shares? (-> shares? boolean?))]
  [shares->list       (-> shares? (listof hotel?))]
  
  [shares-combinable? (-> (listof shares?) boolean?)]
  [*combine-shares  (-> (and/c (listof shares?) shares-combinable?) shares?)]
  [*create-shares   (-> hotel? natural-number/c shares?)]
  [xshare?          (-> any/c boolean?)]
  [xshare<=?        (-> xshare? xshare? boolean?)]
  [shares->xexpr    (-> shares? (and/c (listof xshare?) (sorted xshare<=?)))]
  [xorder?          (-> any/c boolean?)]
  [shares->string   (-> shares? string?)]
  [order->xexpr     (-> shares-order/c xorder?)]
  
  ;; cash 
  [cash?  contract?]
  [CASH0  cash?]
  [string->cash (-> string? (maybe/c cash?))]
  [cash->string (-> cash? string?)]
  
  ;; buying shares
  [price-per-share
   ;; determine the price per share, 
   ;; given the number of tiles placed for this hotel 
   (-> hotel? natural-number/c (maybe/c cash?))]
  
  ;; merging hotels 
  [bonus
   ;; determine the bonus payment for the majority/minority owner, 
   ;; given the number of tiles placed for the hotel
   (-> (or/c 'majority 'minority) hotel? natural-number/c cash?)])
