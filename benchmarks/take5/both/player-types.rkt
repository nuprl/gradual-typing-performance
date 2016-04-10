#lang typed/racket/base

(provide
  Player%
  Player)

(require
  "basics-types.rkt"
  "deck-types.rkt"
  "card-adapted.rkt"
  "stack-types.rkt"
  typed/racket/class)

(define-type Player%
  (Class
    (init-field
      (n Name)
      (order (-> (Listof Card) (Listof Card)) #:optional))
    (field [my-cards [Listof Card]])
    (name (-> Name))
    (start-round (-> (Listof Card) Void))
    (start-turn (-> Deck Card))
    (choose (-> Deck Stack))))
(define-type Player (Instance Player%))
