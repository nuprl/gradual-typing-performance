#lang typed/racket/base

;; the dealer and supervisor of the deck

(provide
 ;; [Listof Player] -> Dealer
 ;; create a dealer object that connects the players with the deck
 ;; and places the player's chosen cards
 create-dealer)

(require
  "basics-types.rkt"
  "card-pool-types.rkt"
  "dealer-types.rkt"
  "deck-types.rkt"
  "player-types.rkt"
  typed/racket/class)

;; ---------------------------------------------------------------------------------------------------

;(require/typed "deck.rkt"
;)
;
;(require/typed "stack.rkt"
;  (bulls
;)
;
;(require/typed "card.rkt"
;)
;
;(require/typed "card-pool.rkt"
;)


;; Note to self: the types for the below descriptions are used out of scope for now
;; in a file-module they come back into scope 
#;(define-local-member-name
  ;; fields:
  
  internal%
  ;; [Listof [Instance Internal%]]
  internals
  
  ;; methods:
  
  ;; N -> Result
  ;; project internals to a list of player names and scores, 
  ;; sorted in increasing order for the latter 
  present-results
  
  ;; -> Boolean
  ;; has any of the internals of this dealer collected more than SIXTYSIX bull points
  any-player-done?
  
  ;; -> Void
  ;; effect: run one complete round of the game for this dealer and its players
  play-round
  
  ;; CardPool -> Void
  ;; effect: hand each player the cards for one round 
  deal-cards 
  
  ;; Deck -> Void
  ;; effect: run one complete turn of a round game for this dealer, its players, & current deck
  play-turn
  
  ;; Deck [Listof [List Internal Card]] -> Void
  ;; effect: place each player's designed card on this dealer's stacks (from the deck)
  ;; assume: the second argument are pairs of cards and players, sorted in the order to be placed  
  place-cards)

(: create-dealer (-> (Listof Player) Dealer))
(define (create-dealer players)
  (new dealer% [players players]))

(define dealer% : Dealer%
  (class object%
    (init-field
     ;; [Listof Player]
     players)
    
    (super-new)
    
    ;; ---------------------------------------------------------------------------------------------
    ;; representing players for the dealer
    (types Player%)
    
    (define-type Internal
      (Class
       (init-field
        ;; Player
        ;; the external player that this internal representation wraps 
        player)
       
       ;; N
       ;; the number of bulls that this wrapped player lost 
       (field [my-bulls N])
       
       ;; -> N
       ;; retrieve my-bulls from this wrapped player
       bulls
       
       ;; N -> Void
       ;; add n to this wrapped player's bulls 
       add-score))
    
    (field
     [internal%
      (class player%
        (init-field player)
        (super-new [n (send player name)])
        
        (field [my-bulls 0])
        (define/public (bulls) my-bulls)
        (define/public (add-score n)
          (set! my-bulls (+ my-bulls n))))])
    
    ;; ---------------------------------------------------------------------------------------------
    ;; running a game 
    
    ;; [Listof Internal]
    (field [internals (for/list ((p players)) (new internal% [player p]))])
    
    (define/public (play-game (shuffle values) (faces (lambda () MIN-BULL)))
      (displayln '(welcome to 6 takes!))
      (displayln `(configuration ,(configuration)))
      (define n (length players))
      (when (> (+ (* n HAND) STACKS) FACE)
        (error 'play-game "cannot play with ~a players; more cards needed" n))
      
      (let play-game ([i 1])
        (play-round shuffle faces)
        (if (any-player-done?)
            (present-results i)
            (play-game (+ i 1)))))
    
    (define/public (present-results i)
      (define sorted (sort internals < #:key (lambda (i) (send i bulls))))
      `((after round ,i)
        ,(for/list ([p sorted])
           `(,(send p name) ,(send p bulls)))))
    
    (define/public (any-player-done?)
      (for/or ((p internals))
        (> (send p bulls) SIXTYSIX)))
    
    (define/public (play-round shuffle faces)
      (define card-pool (create-card-pool shuffle faces))
      (define deck (create-deck card-pool))
      (deal-cards card-pool)
      (for ((p HAND))
        (play-turn deck)))
    
    (define/public (deal-cards card-pool)
      (for ((p internals))
        (send p start-round (send card-pool draw-hand))))
    
    (define/public (play-turn deck)
      (define played-cards 
        (for/list ((p internals))
          (list p (send p start-turn deck))))
      (define sorted-played-cards
        (sort played-cards < #:key (compose card-face second)))
      (place-cards deck sorted-played-cards))
    
    (define/public (place-cards deck sorted-player-cards)
      (for ((p+c sorted-player-cards))
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
           (send player add-score bulls)])))))

