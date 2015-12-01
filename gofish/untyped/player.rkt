#lang racket/base

(provide
 check-done
 check-hand
 create-player
 find-initial-matches
 fishing
 go-fish?
 player?
 player-callback
 player-r
 player-hand
 simulate-player
)

;; -----------------------------------------------------------------------------

(require
  racket/class
  "../base/cards.rkt"
)
(require (only-in "region.rkt"
  make-hand-region
  make-discard-region
  make-discard-count-region 
))
(require (only-in "deck.rkt"
  deal
))

;; =============================================================================

(define DEAL-COUNT 7)
(define GAME-OVER-MESSAGE "GAME OVER")
(define MATCH-MESSAGE "Match!")

;; Callbacks communicate back to the main loop via these
;(define something-happened (make-semaphore 1))
(define go-fish? #t)

;; -----------------------------------------------------------------------------

;; Player record
(define-struct player (
  r
  hand-r
  discard-r
  count-r
  hand
  discarded
  tried
) #:mutable) ; memory for simulating players

;; -----------------------------------------------------------------------------

;; Player setup
(define (create-player t deck r discard-callback)
  (define top (car (unbox deck)))
  (define ch (send top card-height))
  (define cw (send top card-width))
  (let ([p (make-player
            r
            (make-hand-region r cw ch)
            (make-discard-region r cw ch)
            (make-discard-count-region r 0 discard-callback cw ch)
            (deal deck DEAL-COUNT)
            null
            null)])
    (send t add-region r)
    (send t add-region (player-count-r p))
    (for-each (lambda (card)
                (send t card-to-front card)) (reverse (player-hand p))) 
    (send t move-cards-to-region (player-hand p) (player-hand-r p))
    p))

;; Function for trying to get a card from another player
(define (ask-player-for-match t deck getter giver card)
  (let* ([h (player-hand giver)]
         [found (find-equiv card h)])
    (if found
      (begin
        ;; The giver player has a matching card - give it to the getter
        (set-player-hand! giver (remq found h))
        (set-player-hand! getter (cons found (player-hand getter)))
        ;; Make sure the matching cards are face-up and pause for the user
        (send t cards-face-up (list found card))
        ;; Move the cards around
        (check-hand t deck getter card)
        (rearrange-cards t deck giver)
        #t)
      ;; The giver player doesn't have it - Go Fish!
      #f)))

;; Function to simulate a player
(define (simulate-player t deck player other-player* k)
  ;; Try cards in the players hand that haven't been tried
  (let ([cards-to-try (remq* (player-tried player) (player-hand player))])
    (if (null? cards-to-try)
      (begin
        ;; No cards to try. Reset the history and start over
        (set-player-tried! player null)
        (simulate-player t deck player other-player* k))
      ;; Pick a random card and a random opponent
      (let ([c (list-ref cards-to-try (random (length cards-to-try)))]
            [o (list-ref other-player* (random 2))])
        (set-player-tried! player (cons c (player-tried player)))
        ;; Show you the card-to-ask
        (send t flip-card c)
        ;; Hilight player-to-ask
        (send t hilite-region (player-r o))
        ;; Wait a moment
        ;(sleep 0.3)
        ;; Unhilight player-to-ask
        (send t unhilite-region (player-r o))
        (if (ask-player-for-match t deck player o c)
          ;; Got it - go again
          (check-done t
           (lambda ()
             (simulate-player t deck player other-player* k))
           (cons player other-player*))
          ;; Go fish
          (begin
            ;; Wait a bit, then turn the asked-for card back over
            ;(sleep 0.3)
            (send t flip-card c)
            (if (null? (unbox deck))
              ;; No more cards; pass
              (k)
              (begin
                ;; Draw a card
                (set-player-hand! player (append (deal deck 1) (player-hand player)))
                (rearrange-cards t deck player)
                (if (check-hand t deck player (car (player-hand player)))
                  ;; Drew a good card - keep going
                  (check-done t
                   (lambda ()
                     (simulate-player t deck player other-player* k))
                   (cons player other-player*))
                  ;; End of our turn
                  (k))))))))))

;; Function to check for end-of-game
(define (check-done t k player*)
  (if (ormap (lambda (p) (null? (player-hand p))) player*)
    (send t set-status GAME-OVER-MESSAGE)
    (k)))

;; Function to search for an equivalent card
(define (find-equiv card hand)
  (ormap (lambda (c)
           (and (not (eq? c card))
                (= (send card get-value) (send c get-value))
                c))
         hand))

;; Function to check for a match involving `card' already in the player's hand
(define (check-hand t deck player card)
  (let* ([h (player-hand player)]
         [found (find-equiv card h)])
    (if found
      (begin
        ;; Make sure the matching cards are face-up and pause for the user
        (send t cards-face-up (list found card))
        (send t set-status MATCH-MESSAGE)
        ;; The players has a match! Move the card from the player's hand
        ;;  to his discard pile
        (set-player-hand! player (remove* (list card found) h))
        (set-player-discarded! player
                               (list* found card (player-discarded player)))
        ;; The dicarded cards can no longer be moved
        (send card user-can-move #f)
        (send found user-can-move #f)
        ;; Move the cards to their new places
        (rearrange-cards t deck player)
        ;; Slower
        #t)
      #f)))

;;bg; TODO move this? it uses the player stuct, so it's internal for now
;; Function to update the display for a player record
(define (rearrange-cards t deck p)
  (define top (car (unbox deck)))
  ;; Stack cards in 3D first-to-last
  (send t stack-cards (player-discarded p))
  (send t stack-cards (player-hand p))
  ;; Move them to their regions
  (send t move-cards-to-region (player-discarded p) (player-discard-r p))
  (send t move-cards-to-region (player-hand p) (player-hand-r p))
  ;; Recreate the counter region to reset the count
  (send t begin-card-sequence)
  (send t remove-region (player-count-r p))
  (set-player-count-r! p (make-discard-count-region 
                          (player-r p)
                          (/ (length (player-discarded p)) 2)
                          (region-callback (player-count-r p))
                          (send top card-width)
                          (send top card-height)))
  (send t add-region (player-count-r p))
  (send t end-card-sequence))

;; Look in opponents' initial hands for matches (Since each player gets 7
;; cards, it's impossible to run out of cards this way)
(define (find-initial-matches t deck player)
  (when (ormap (lambda (card) (check-hand t deck player card)) (player-hand player))
    ;; Found a match in the hand
    (find-initial-matches t deck player)))

;; Callback for going fishing
(define (fishing t deck you)
  (lambda (cards)
    (send t flip-card (car (unbox deck)))
    (set-player-hand! you (append (deal deck 1) (player-hand you)))
    (rearrange-cards t deck you)
    #;(semaphore-post something-happened)))

;; Callback for dragging a card to an opponent
(define (player-callback t deck player other)
  (lambda (cards)
    (set! go-fish? (not (ask-player-for-match t deck other player (car cards))))
    #;(semaphore-post something-happened)))

