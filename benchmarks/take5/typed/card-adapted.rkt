 #lang typed/racket/base

 (provide
   Card
   card
   card-face
   card-bulls
   >-face
   --face)

(require "basics-types.rkt")
(require/typed "card.rkt"
 (#:struct card (
  [face : Face]
  [bulls : Bulls]))
 (>-face (-> Card Card Boolean))
 (--face (-> Card Card Natural)))

(define-type Card card)

