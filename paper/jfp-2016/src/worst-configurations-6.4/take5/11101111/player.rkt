#lang typed/racket/base

;; representing the player for an OO version of "6 Nimmt"

(provide
 ;; Name [x [Listof Card] -> [Listof Card]] -> Player
 ;; the default value for the second argument sorts the cards in descending order 
 create-player

 ;; for sub-typing:
 ;; Player%
 player%)

(require
  "basics-types.rkt"
  "card-adapted.rkt"
  "deck-types.rkt"
  "player-types.rkt"
  "stack-types.rkt"
  typed/racket/class
  (only-in racket/list first rest)
)

;; ---------------------------------------------------------------------------------------------------

(: create-player (->* [Name] [(-> (Listof Card) (Listof Card))] Player))
(define (create-player i (order default-order))
  (new player% [n i] [order order]))

(: default-order (-> (Listof Card) (Listof Card)))
(define (default-order loc)
  ((inst sort Card Natural) loc > #:key card-face))

(: player% Player%)
(define player%
  (class object%
    (init-field n
      (order  default-order))

    (field [my-cards '()])

    (define/public (name) n)

    (define/public (start-round loc)
      (set! my-cards (order loc)))

    (define/public (start-turn _d)
      (begin0 (first my-cards)
              (set! my-cards (rest my-cards))))

    (define/public (choose d)
      (define fewest-bulls (send d fewest-bulls))
      fewest-bulls)

    (super-new)))
