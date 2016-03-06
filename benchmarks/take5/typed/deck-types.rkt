#lang typed/racket/base

(provide
  BaseDeck% BaseDeck
  PlayerDeck% PlayerDeck
  DealerDeck% DealerDeck
  Deck% Deck)

(require
  typed/racket/class
  "basics-types.rkt"
  "card-adapted.rkt"
  "stack-types.rkt")

(define-type BaseDeck%
    (Class
      (init-field (cards0 (Listof Card)))
      (field (my-stacks (Listof Stack)))))
(define-type PlayerDeck%
    (Class ;; for player
      (fewest-bulls (-> Stack))))
(define-type DealerDeck%
    (Class ;; for dealer
      (fit (-> Card Stack))
      (push (-> Card Void))
      (replace (-> Stack Card Void))
      (larger-than-some-top-of-stacks? (-> Card Boolean))))
(define-type Deck%
    (Class
      (init-field (cards0 (Listof Card)))
      (field (my-stacks (Listof Stack)))
      (fewest-bulls (-> Stack))
      (fit (-> Card Stack))
      (push (-> Card Void))
      (replace (-> Stack Card Void))
      (larger-than-some-top-of-stacks? (-> Card Boolean))))

(define-type BaseDeck (Instance BaseDeck%))
(define-type PlayerDeck (Instance PlayerDeck%))
(define-type DealerDeck (Instance DealerDeck%))
(define-type Deck (Instance Deck%))
