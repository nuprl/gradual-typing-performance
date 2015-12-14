#lang typed/racket/base

(provide
  Card<%>
  Card*
  Card
  Deck
  Region
  RegionCallback
  Table
  Table<%>
  ;; --
  make-deck
  make-region
  make-table
  set-region-callback!
  region-callback
  region-h
  region-label
  region-w
  region-x
  region-y
  shuffle-list
)

(require
   typed/racket/class
)

(define-type Card<%>
  (Class
   [card-width (-> Natural)]
   [card-height (-> Natural)]
   [user-can-move (-> Boolean Void)]
   [get-value (-> Natural)]
))

(define-type Table<%>
  (Class
   [add-cards (-> Card* Real Real Void)]
   [add-region (-> Region Void)]
   [begin-card-sequence (-> Void)]
   [cards-face-up (-> Card* Void)]
   [card-to-front (-> Card Void)]
   [end-card-sequence (-> Void)]
   [flip-card (-> Card Void)]
   [hilite-region (-> Region Void)]
   [move-card (-> (Instance Card<%>) Real Real Void)]
   [move-cards-to-region (-> Card* Region Void)]
   [remove-region (-> Region Void)]
   [set-status (-> String Void)]
   [stack-cards (-> Card* Void)]
   [table-height (-> Natural)]
   [table-width (-> Natural)]
   [unhilite-region (-> Region Void)]
))
(define-type Table (Instance Table<%>))

(require/typed "../base/cards.rkt"
 [make-deck (-> Card*)]
 [#:opaque Region region?]
 ;; Because 'region' is not really a struct ...
 ;; It's a 'make-struct-type' wrapped in a macro
 [make-region (-> Real Real Real Real (Option String) RegionCallback Region)]
 [make-table (-> String Real Real Table)]
 [region-x (-> Region Real)]
 [region-y (-> Region Real)]
 [region-w (-> Region Nonnegative-Real)]
 [region-h (-> Region Nonnegative-Real)]
 [region-label (-> Region (Option String))]
 [region-callback (-> Region RegionCallback)]
 [set-region-callback! (-> Region RegionCallback Void)]
 [shuffle-list (All (A) (-> (Listof A) Natural (Listof A)))]
)
(define-type RegionCallback (Option (-> Card* Any)))
(define-type Card (Instance Card<%>))
(define-type Card* (Listof Card))
(define-type Deck (Boxof Card*))
