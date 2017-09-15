#lang typed/racket/base

(provide
  Result
  Dealer%
  Dealer
  ;; --
  Internal%
  Internal
)

(require
  typed/racket/class
  "basics-types.rkt"
  "card.rkt"
  "player-types.rkt"
)
(define-type Card card)

;; -----------------------------------------------------------------------------

(define-type Result (List (List Symbol Natural) (Listof (List Name Natural))))

;; (sad face)
(define-type Internal%
  (Class
    #:implements Player%
    (init-field [player Player])
    (field [my-bulls Natural])
    (bulls (-> Natural))
    (add-score (-> Natural Void))))
(define-type Internal (Instance Internal%))

(define-type Dealer%
  (Class
    (init-field (players (Listof Player)))
    (field
     (internal% Internal%)
     (internals (Listof Internal)))
    (present-results (-> Natural Result))
    (any-player-done? (-> Boolean))
    (play-round (-> (-> (Listof Card) (Listof Card)) (-> Bulls) Void))
    (play-game (->* ()  ((-> (Listof Card) (Listof Card)) (-> Bulls)) Result))))
(define-type Dealer (Instance Dealer%))
