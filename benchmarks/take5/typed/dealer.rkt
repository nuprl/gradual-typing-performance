#lang typed/racket/base

;; the dealer and supervisor of the deck

(provide
 ;; [Listof Player] -> Dealer
 ;; create a dealer object that connects the players with the deck
 ;; and places the player's chosen cards
 create-dealer)

(require
  "basics-types.rkt"
  "card-adapted.rkt"
  "card-pool-types.rkt"
  "dealer-types.rkt"
  "deck-types.rkt"
  "player-types.rkt"
  racket/list
  require-typed-check
  typed/racket/class)

(require/typed/check "basics.rkt"
  (FACE  Natural)
  (FIVE  Natural)
  (STACKS Natural)
  (SIXTYSIX Natural)
  (HAND  Natural)
  (MIN-BULL Bulls)
  (MAX-BULL Bulls)
  (configuration (-> (Listof (List Symbol Natural))))
)
(require/typed/check "card-pool.rkt"
  (create-card-pool (-> (-> (Listof Card) (Listof Card)) (-> Bulls) CardPool))
)
(require/typed/check "deck.rkt"
  (create-deck (-> CardPool Deck))
)
(require/typed/check "player.rkt"
  (player% Player%)
)

;; ---------------------------------------------------------------------------------------------------

;; TODO should not need to supply this
(: default-order (-> (Listof Card) (Listof Card)))
(define (default-order loc)
  ((inst sort Card Natural) loc > #:key card-face))

(: default-faces (-> Bulls))
(define (default-faces)
  MIN-BULL)

;; Note to self: the types for the below descriptions are used out of scope for now
;; in a file-module they come back into scope 

(: create-dealer (-> (Listof Player) Dealer))
(define (create-dealer players)
  (new dealer% [players players]))

(define dealer% : Dealer%
  (class object%
    (init-field
     (players : (Listof Player)))

    (super-new)

    (field
     [internal% : Internal%
      (class player%
        (init-field player)
        (super-new [n (send player name)] [order default-order])
        (field [my-bulls 0])
        (define/public (bulls) (get-field my-bulls this))
        (define/public (add-score n)
          (set-field! my-bulls this (+ n (get-field my-bulls this)))))]
     [internals (for/list : (Listof Internal)
                          ([p : Player (in-list (get-field players this))])
                  (new internal% [player p]))])

    ;; ---------------------------------------------------------------------------------------------
    ;; running a game 

    (define/public (play-game (shuffle values) (faces default-faces))
      (define n (length (get-field internals this)))
      (when (> (+ (* n HAND) STACKS) FACE)
        (error 'play-game "cannot play with ~a players; more cards needed" n))

      (let play-game : Result ([i : Natural 1])
        (play-round shuffle faces)
        (if (any-player-done?)
            (present-results i)
            (play-game (+ i 1)))))

    (define/public (present-results i)
      (define sorted
        ((inst sort Internal Natural)
         (get-field internals this) < #:key (lambda ([i : Internal]) (send i bulls))))
      `((after-round ,i)
        ,(for/list : (Listof (List Name Natural))
            ([p : Internal (in-list sorted)])
           `(,(send p name) ,(send p bulls)))))

    (define/public (any-player-done?)
      (for/or : Boolean
              ((p : Internal (in-list (get-field internals this))))
        (> (send p bulls) SIXTYSIX)))

    (define/public (play-round shuffle faces)
      (define card-pool (create-card-pool shuffle faces))
      (define deck (create-deck card-pool))
      (deal-cards card-pool)
      (for ((p HAND))
        (play-turn deck)))

    (: deal-cards (-> CardPool Void))
    (define/private (deal-cards card-pool)
      (for ((p : Internal (in-list (get-field internals this))))
        (send p start-round (send card-pool draw-hand))))

    (: play-turn (-> Deck Void))
    (define/private (play-turn deck)
      (define played-cards
        (for/list : (Listof (List Internal Card))
                  ((p : Internal (in-list (get-field internals this))))
          (list p (send p start-turn deck))))
      (define sorted-played-cards
        ((inst sort (List Internal Card) Face) played-cards < #:key (lambda ([x : (List Internal Card)]) (card-face (second x)))))
      (place-cards deck sorted-played-cards))

    (: place-cards (-> Deck (Listof (List Internal Card)) Void))
    (define/private (place-cards deck sorted-player-cards)
      (for ((p+c : (List Internal Card) (in-list sorted-player-cards)))
        (define player (first p+c))
        (define card (second p+c))
        (cond
          [(send deck larger-than-some-top-of-stacks? card)
           (define closest-fit-stack (send deck fit card))
           (cond
             [(< (length closest-fit-stack) FIVE)
              (send deck push card)]
             [(= (length closest-fit-stack) FIVE)
              (define bulls (send deck replace closest-fit-stack card))
              (send player add-score bulls)])]
          [else ;; the tops of all stacks have larger face values than card
           (define chosen-stack (send player choose deck))
           (define bulls (send deck replace chosen-stack card))
           (send player add-score bulls)])))

))
