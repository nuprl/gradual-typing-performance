#lang typed/racket/base
(random-seed 9)

;; -----------------------------------------------------------------------------

(require
  "../base/games-cards-adapted.rkt"
  benchmark-util
  typed/racket/class
)
(require/typed/check "deck.rkt"
  (init-deck (-> Natural Deck))
  (wiggle-top-card (-> Table Deck Void))
)
(require/typed/check "player.rkt"
  (#:opaque Player player?)              
  (check-done (-> Table (-> Void) (Listof Player) Void))
  (check-hand (-> Table Deck Player Card Boolean))
  (create-player (-> Table Deck Region RegionCallback Player))
  (find-initial-matches (-> Table Deck Player Void))
  (fishing (-> Table Deck Player (-> Card* Void)))
  (go-fish? Boolean)
  (player-callback (-> Table Deck Player Player (-> Card* Void)))
  (player-hand (-> Player Card*))
  (player-r (-> Player Region))
  (simulate-player (-> Table Deck Player (Listof Player) (-> Void) Void))
)

;; =============================================================================

(define MARGIN 10)
(define NUM-CARDS 7)
(define PLAYER-1-NAME "Opponent 1")
(define PLAYER-2-NAME "Opponent 2")
(define YOUR-NAME "You")
(define YOUR-TURN-MESSAGE "Your turn.  (Drag a match to your discard box or drag a card to an opponent.)")
(define GO-FISH-MESSAGE "Go Fish!  (Drag a card from the center deck to your box.)")

(define (main)
  ;; Create deck
  (define deck (init-deck NUM-CARDS))
  (define top (car (unbox deck)))
  (define ch (send top card-height))
  (define cw (send top card-width))
  ;; Setup table
  (define t (make-table "Go Fish" 8 4.5))
  (define w (send t table-width))
  (define h (send t table-height))
  (define pw (- (/ (- w cw) 2) (* 2 MARGIN)))
  (define ph (- (/ (- h (/ ch 3)) 2) (* 2 MARGIN)))
  (send t add-cards (unbox deck) (/ (- w cw) 2) (- (/ (- h ch) 2) (/ ch 3)))
  ;; Define the initial regions
  (define player-1-region
    (make-region MARGIN MARGIN pw ph PLAYER-1-NAME void))
  (define player-2-region
    (make-region (- w MARGIN pw) MARGIN pw ph PLAYER-2-NAME void))
  (define you-region
    (make-region MARGIN (- h MARGIN ph) (- w (* 2 MARGIN)) ph YOUR-NAME void))
  ;; Create players
  (define you      (create-player t deck you-region #f))
  (define player-1 (create-player t deck player-1-region #f))
  (define player-2 (create-player t deck player-2-region #f))
  (define player* (list you player-1 player-2))
  (find-initial-matches t deck player-1)
  (find-initial-matches t deck player-2)
  ;; Main loop
  (let loop : Void ()
    (set-region-callback! (player-r you) #f)
    (set-region-callback! (player-r player-1) (player-callback t deck player-1 player-2))
    (set-region-callback! (player-r player-2) (player-callback t deck player-2 player-1))
    (send t set-status YOUR-TURN-MESSAGE)
    ;(yield something-happened)
    (if go-fish?
      (begin
        (if (if (null? deck)
              ;; No more cards; pass
              #f
              ;; Draw a card (wait for the user to drag it)
              (begin 
                (send t set-status GO-FISH-MESSAGE)
                     (wiggle-top-card t deck)
                     (set-region-callback! (player-r player-1) #f)
                     (set-region-callback! (player-r player-2) #f)
                     (set-region-callback! (player-r you) (fishing t deck you))
                     (send (car (unbox deck)) user-can-move #t)
                     ;(yield something-happened)
                     (check-hand t deck you (car (player-hand you)))))
          (check-done t loop player*)
          (begin 
            (send t set-status PLAYER-1-NAME)
                 (simulate-player t deck
                  player-1 (list you player-2)
                  (lambda ()
                    (send t set-status PLAYER-2-NAME)
                    (simulate-player t deck player-2 (list you player-1) loop))))))
      (check-done t loop player*))))

(time (main))
