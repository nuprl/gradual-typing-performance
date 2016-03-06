#lang racket

;; representing the deck of STACKS stack on the table 

(require "stack.rkt" "card-pool.rkt" "card.rkt" "../base/utility.rkt")

(types Stack CardPool Card)

(provide
 (type Deck [Instance Deck%])
 (type Deck%
       (U (Class
           (init-field cards0 [Listof Card])
           (field [my-stacks [Listof Stack]]))
          (Class ;; for Player 
           ;; -> Stack
           ;; retrieve the stack whose cards show the fewest bulls 
           fewest-bulls)
          (Class ;; for Dealer 
           ;; Card -> Stack
           ;; (fit c)
           ;; find the stack in this deck whose top card is closest in face value to c
           fit
           ;; Card -> Void
           ;; (push c) -- push c onto the best-fitting stack in this deck
           push
           ;; Stack Card -> Void
           ;; (replace s c) -- replace s in this deck with (create-stack c)
           ;; assumes s is in this d (compare top, all cards are unique)
           replace
           ;; Card -> Boolean 
           ;; (larger-than-some-top-of-stacks? c)
           ;; is the face of c larger than any of the faces at the tops of this deck's stacks?
           larger-than-some-top-of-stacks?)))
 
 ;; CardPool -> Deck 
 create-deck)

;; ---------------------------------------------------------------------------------------------------
(require "basics.rkt")
(module+ test (require rackunit))

(define-local-member-name
  cards0
  ;; my-stacks
  ;; Card (U Card Stack) -> N
  ;; effect: find stack with top0 as card in this deck and
  ;; -- add card to it
  ;; or 
  ;; -- use given stack in its place
  ;; produce the total bulls of cards on the modified stack 
  replace-stack)

(define (create-deck card-pool)
  (define deck% (for-player (for-dealer base-deck%)))
  (define cards (build-list STACKS (lambda (_) (send card-pool draw-card))))
  (new deck% [cards0 cards]))

;; I have chosen to represent these views via two mixins:
;;   -- for-player
;;   -- for-dealter

;; Class[my-stacks field] -> Class[my-stacks field and fewest-bulls method]
(define (for-player deck%)
  (class deck%
    (inherit-field my-stacks)
    (super-new)
    
    (define/public (fewest-bulls)      
      (define stacks-with-bulls
        (for/list ((s my-stacks))
          (list s (bulls s))))
      (first (argmin second stacks-with-bulls)))))

;; Class[cards0 field] -> Class[fit, push, replace & larger-than-some-top-of-stacks? methods]
(define (for-dealer deck%)
  (class deck%
    (inherit-field cards0)
    (super-new)
    
    ;; [Listof Stack]
    (field [my-stacks (map list cards0)])
    
    (define/public (fit c)
      (define (distance stack)
        (define d (first stack))
        (if (>-face c d) (--face c d) (+ FACE 1)))
      (argmin distance my-stacks))
    
    (define/public (push c)
      (define s0 (fit c))
      (void (replace-stack (first s0) c)))
    
    (define/public (replace s c)
      (replace-stack (first s) (list c)))
    
    (define/public (replace-stack top0 c)
      (define result 0)
      (set! my-stacks 
            (for/list ((s my-stacks))
              (cond
                [(equal? (first s) top0)
                 (set! result (bulls s))
                 (if (cons? c) c (cons c s))]
                [else s])))
      result)
    
    (define/public (larger-than-some-top-of-stacks? c)
      (for/or ((s my-stacks))
        (>-face c (first s))))))

;; Class[cards0 field]
(define base-deck%
  (class object%
    (init-field
     ;; [Listof Card]
     ;; the tops of the initial stacks (for a round)
     cards0)
    
    (super-new)))

(module+ test
  (require "stack.rkt" "card.rkt" "basics.rkt")
  
  (define (setup-deck-with-stacks cards)
    (define _card-pool (create-card-pool))
    (define deck (create-deck _card-pool))
    (define stacks (map create-stack cards))
    (set-field! my-stacks deck stacks)
    deck)
  
  ;; setup: 
  (define base 10) ; > 1
  (define card1 (card 1 MIN-BULL))
  (define (deck0)
    (define first-stacks-cards
      (build-list STACKS (lambda (i) (card (+ i base) MIN-BULL))))
    (setup-deck-with-stacks first-stacks-cards))
  (define next-card0 (card (+ STACKS base 1) MIN-BULL))
  (define first-card0 (card (+ 0 base) MIN-BULL))
  (define last-card0 (card (+ (- STACKS 1) base) MIN-BULL))
  (define last-stack0 (create-stack last-card0))
  
  ;; tests
  (check-true (send (deck0) larger-than-some-top-of-stacks? next-card0))
  (check-equal? (send (deck0) fit next-card0) last-stack0)
  
  (check-equal? (send (deck0) fewest-bulls) (create-stack first-card0))
  
  (check-equal? (last
                 (let* ([d (deck0)]
                        [_ (send d push next-card0)])
                   (get-field my-stacks d)))
                (push next-card0 (create-stack last-card0)))
  
  (check-equal? (let* ([d (deck0)]
                       [_ (send d push next-card0)])
                  (send d fewest-bulls))
                (create-stack first-card0))
  
  (check-equal? (last
                 (let* ([d (deck0)]
                        [_ (send d replace last-stack0 card1)])
                   (get-field my-stacks d)))
                (create-stack card1)))
