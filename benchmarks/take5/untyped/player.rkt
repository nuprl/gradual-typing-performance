#lang racket

;; representing the player for an OO version of "6 Nimmt"

(require "deck.rkt" "stack.rkt" "card.rkt" "../base/utility.rkt")
(types Deck Stack Card)

(provide
 (type Name N)
 (type Player [Instanceof Player%])
 (type Player%
       (Class
        (init-field
         ;; N
         ;; the name of this player 
         n)
        
        ;; the cards in the player's hands 
        (field [my-cards [Listof Card]])
        
        ;; -> N
        ;; retrieve the index of this player 
        name
        
        ;; [Listof Card] -> Void
        ;; effect: start this player for a round of the game 
        ;; assume: call once per turn 
        start-round
        
        ;; Deck -> Card
        ;; effect: start this player on a turn of a round of the game 
        ;; assume: call only once per turn, don't call when round is over
        ;; note: Deck is ignored here 
        start-turn
        
        ;; Deck -> Stack
        ;; choose the stack during a turn for this player 
        choose))
 
 ;; Name [x [Listof Card] -> [Listof Card]] -> Player
 ;; the default value for the second argument sorts the cards in descending order 
 create-player
 
 ;; for sub-typing:
 ;; Player%
 player%)

;; ---------------------------------------------------------------------------------------------------
(module+ test (require rackunit))

(define (create-player i (order  (lambda (loc) (sort loc > #:key card-face))))
  (new player% [n i] [order order]))

(define player%
  (class object%
    (init-field n (order  (lambda (loc) (sort loc > #:key card-face))))
    
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

(module+ test
  (require "deck.rkt"
           (only-in "stack.rkt" create-stack)
           "card-pool.rkt"
           "card.rkt"
           "basics.rkt")
  
  (define hand (build-list HAND (lambda (i) (card (+ i 1) 1))))
  
  (define player (create-player 1))
  (send player start-round hand)
  (check-equal? (send player start-turn 'deck)
                ;; assuming play uses > to sort its hand 
                (last hand))
  
  (define player2 (create-player 2))
  (send player2 start-round hand)
  (define d (create-deck (create-card-pool values (lambda () MIN-BULL))))
  (check-equal? (send player2 choose d) (create-stack (card 1 MIN-BULL))))
