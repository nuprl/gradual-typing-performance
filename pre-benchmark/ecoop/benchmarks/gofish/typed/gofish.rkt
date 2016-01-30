#lang typed/racket
(require "require-typed-check.rkt" "typed-base.rkt" (except-in typed/racket/gui pasteboard%) typed/racket/class)

(require/typed/if "region-typed.rkt" "region.rkt")
(require/typed/check "make-cards.rkt" [deck-of-cards (Listof (Instance Card%))])
(require/typed/check "classes.rkt" [table% Table%] [shuffle-list (All (X) (-> (Listof X) Natural (Listof X)))])

;; This needs to be internal to the typed cards library
;; need to maybe add it to a local typed/games/cards import
(define make-table
  (lambda ([title : String "Cards"] [w : Nonnegative-Real 7] [h : Nonnegative-Real 3])
    (make-object table% title w h)))

(: make-deck (-> (Listof (Instance Card%))))
(define (make-deck)
  ((inst map (Instance Card%) (Instance Card%)) (lambda ([l : (Instance Card%)]) (send l copy)) deck-of-cards))

;; Player record
(define-struct player ([r : Region] [hand-r : Region] [discard-r : Region] [count-r : Region] ; regions
                       [hand : (Listof (Instance Card%))] [discarded : (Listof (Instance Card%))] ; cards
                       [tried : (Listof (Instance Card%))]) #:mutable) ; memory for simulating players

;; Player names
(define PLAYER-1-NAME "Opponent 1")
(define PLAYER-2-NAME "Opponent 2")
(define YOUR-NAME "You")

;; Initial card count
(define DEAL-COUNT 7)

;; Messages
(define YOUR-TURN-MESSAGE
  "Your turn.  (Drag a match to your discard box or drag a card to an opponent.)")
(define GO-FISH-MESSAGE
  "Go Fish!  (Drag a card from the center deck to your box.)")
(define MATCH-MESSAGE "Match!")
(define GAME-OVER-MESSAGE "GAME OVER")

;; Region layout constants
(define MARGIN 10)
(define SUBMARGIN 10)
(define LABEL-H 15)

;; Randomize (NOTE: not needed since the game is deterministic for the benchmark)
;(random-seed (modulo (current-milliseconds) 10000))

;; Set up the table
(define t (make-table "Go Fish" 8 4.5))
(define status-pane (send t create-status-pane))
(send t add-scribble-button status-pane 
      '(lib "games/scribblings/games.scrbl") "gofish")
;(send t show #t) ;; (NOTE: GUI is disabled)
(send t set-double-click-action void)
(send t set-button-action 'left 'drag-raise/one)
(send t set-button-action 'middle 'drag/one)
(send t set-button-action 'right 'drag/one)

;; Get table width & height
(define w (send t table-width))
(define h (send t table-height))

;; Set up the cards
(define: deck : (Listof (Instance Card%)) (shuffle-list (make-deck) 7))
(for-each (lambda ([card : (Instance Card%)])
            (send card snap-back-after-move #t)
            (send card user-can-flip #f))
          deck)

;; Function for dealing or drawing cards
(: deal (-> Integer (Listof (Instance Card%))))
(define (deal n)
  (let loop : (Listof (Instance Card%)) ([n : Integer n] [d : (Listof (Instance Card%)) deck])
    (if (zero? n)
      (begin (set! deck d) null)
      (cons (car d) (loop (sub1 n) (cdr d))))))

;; Card width & height
(define: cw : Nonnegative-Real (send (car deck) card-width))
(define: ch : Nonnegative-Real (send (car deck) card-height))

;; Put the cards on the table
(send t add-cards deck (/ (- w cw) 2) (- (/ (- h ch) 2) (/ ch 3)))

;; Player region size
(define: pw : Real (- (/ (- w cw) 2) (* 2 MARGIN)))
(define: ph : Real (- (/ (- h (/ ch 3)) 2) (* 2 MARGIN)))

;; Region-makers
(: make-hand-region (-> Region Region))
(define (make-hand-region r)
  (define m SUBMARGIN)
  (make-region (+ m (region-x r)) (+ LABEL-H m (region-y r))
               (- (region-w r) (* 3 m) cw)
               (- (region-h r) LABEL-H (* 2 m))
               #f #f))
(: make-discard-region (-> Region Region))
(define (make-discard-region r)
  (make-region (- (+ (region-x r) (region-w r)) SUBMARGIN cw)
               (- (+ (region-y r) (region-h r)) SUBMARGIN ch)
               cw ch
               #f #f))
(: make-discard-count-region (-> Region Real (Option (-> (Listof (Instance Card%)) Any)) Region)) 
(define (make-discard-count-region r c cb)
  (make-region
   (- (+ (region-x r) (region-w r)) SUBMARGIN cw (/ SUBMARGIN 2))
   (- (+ (region-y r) (region-h r)) SUBMARGIN ch LABEL-H (/ SUBMARGIN 2))
   (+ cw SUBMARGIN) (+ ch LABEL-H SUBMARGIN)
   (number->string c)
   cb))

;; Define the initial regions
(define player-1-region
  (make-region MARGIN MARGIN pw ph PLAYER-1-NAME void))
(define player-2-region
  (make-region (- w MARGIN pw) MARGIN pw ph PLAYER-2-NAME void))
(define you-region
  (make-region MARGIN (- h MARGIN ph) (- w (* 2 MARGIN)) ph YOUR-NAME void))

;; Player setup
(: create-player (-> Region (Option (-> (Listof (Instance Card%)) Any)) player))
(define (create-player r discard-callback)
  (let ([p (make-player
            r
            (make-hand-region r)
            (make-discard-region r)
            (make-discard-count-region r 0 discard-callback)
            (deal DEAL-COUNT)
            null
            null)])
    (send t add-region r)
    (send t add-region (player-count-r p))
    (for-each (lambda ([card : (Instance Card%)])
                (send t card-to-front card)) (reverse (player-hand p)))
    (send t move-cards-to-region (player-hand p) (player-hand-r p))
    p))

(define player-1 (create-player player-1-region #f))
(define player-2 (create-player player-2-region #f))
(define you (create-player you-region #f))

;; Note: GUI disabled
#;(define you (create-player you-region
                           ;; Dragging to your discard pile checks to see if
                           ;; the card makes a match:
                           (lambda (cards)
                             (check-hand you (car cards))
                             (send t set-status YOUR-TURN-MESSAGE))))

;; More card setup: Opponents's cards and deck initially can't be moved
(for-each (lambda ([card : (Instance Card%)]) (send card user-can-move #f))
          (append (player-hand player-1) (player-hand player-2) deck))

;; More card setup: Show your cards
(send t flip-cards (player-hand you))

;; Function to update the display for a player record
(: rearrange-cards (-> player Void))
(define (rearrange-cards p)
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
                          (player-r p) (/ (length (player-discarded p)) 2)
                          (region-callback (player-count-r p))))
  (send t add-region (player-count-r p))
  (send t end-card-sequence))

;; Function to search for an equivalent card
(: find-equiv (-> (Instance Card%) (Listof (Instance Card%)) (Option (Instance Card%))))
(define (find-equiv card hand)
  (ormap (lambda ([c : (Instance Card%)])
           (and (not (object=? c card)) ; changing this to use object=? because of contracts
                (= (assert (send card get-value) number?) (assert (send c get-value) number?)) ; maybe this should just be equal? instead
                c))
         hand))

;; Function to check for a match involving `card' already in the player's hand
(: check-hand (-> player (Instance Card%) Boolean))
(define (check-hand player card)
  (let* ([h (player-hand player)]
         [found (find-equiv card h)])
    (if found
      (begin
        ;; Make sure the matching cards are face-up and pause for the user
        (send t cards-face-up (list found card))
        (send t set-status MATCH-MESSAGE)
        ;; The players has a match! Move the card from the player's hand
        ;;  to his discard pile
        (set-player-hand! player (remove* (list card found) h object=?))
        (set-player-discarded! player
                               (list* found card (player-discarded player)))
        ;; The dicarded cards can no longer be moved
        (send card user-can-move #f)
        (send found user-can-move #f)
        ;; Move the cards to their new places
        (rearrange-cards player)
        ;; Slower
        #t)
      #f)))

;; Function to enable/disable moving your cards
(: enable-your-cards (-> Boolean Void))
(define (enable-your-cards on?)
  (for-each (lambda ([c : (Instance Card%)]) (send c user-can-move on?)) (player-hand you)))

;; Callbacks communicate back to the main loop via these
(define something-happened (make-semaphore 1))
(define: go-fish? : Boolean #f)

;; Function for trying to get a card from another player
(: ask-player-for-match (-> player player (Instance Card%) Boolean))
(define (ask-player-for-match getter giver card)
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
        (check-hand getter card)
        (rearrange-cards giver)
        #t)
      ;; The giver player doesn't have it - Go Fish!
      #f)))

;; Callback for dragging a card to an opponent
(: player-callback (->* (player) (player) (-> (Listof (Instance Card%)) Void)))
(define (player-callback player [oplayer you])
  (lambda (cards)
    (set! go-fish? (not (ask-player-for-match oplayer player (car cards))))
    (semaphore-post something-happened)))

;; Visual info to go fish
(define wiggle-top-card
  (lambda ()
    (let ([top (car deck)]
          [x (/ (- w cw) 2)]
          [y (- (/ (- h ch) 2) (/ ch 3))])
      (send t move-card top (- x 10) y)
      (send t move-card top (+ x 10) y)
      (send t move-card top x y))))

;; Callback for going fishing
(define fishing
  (lambda (cards)
    (send t flip-card (car deck))
    (set-player-hand! you (append (deal 1) (player-hand you)))
    (rearrange-cards you)
    (semaphore-post something-happened)))

;; Function to simulate a player
(: simulate-player (->* (player player (-> Void)) (#:other player) Void))
(define (simulate-player player other-player #:other [other-player-2 you] k)
  ;; Try cards in the players hand that haven't been tried
  (let ([cards-to-try (remq* (player-tried player) (player-hand player))])
    (if (null? cards-to-try)
      (begin
        ;; No cards to try. Reset the history and start over
        (set-player-tried! player null)
        (simulate-player player other-player #:other other-player-2 k))
      ;; Pick a random card and a random opponent
      (let ([c (list-ref cards-to-try (random (length cards-to-try)))]
            [o (list-ref (list other-player-2 other-player) (random 2))])
        (set-player-tried! player (cons c (player-tried player)))
        ;; Show you the card-to-ask
        (unless (eq? player you)
          (send t flip-card c))
        ;; Hilight player-to-ask
        (send t hilite-region (player-r o))
        ;; Wait a moment
        (sleep 0.3)
        ;; Unhilight player-to-ask
        (send t unhilite-region (player-r o))
        (if (ask-player-for-match player o c)
          ;; Got it - go again
          (check-done
           (lambda ()
             (simulate-player player other-player #:other other-player-2 k)))
          ;; Go fish
          (begin
            (sleep 0.3)
            (unless (eq? player you)
              (send t flip-card c))
            (if (null? deck)
              ;; No more cards; pass
              (k)
              (let ()
                ;; Draw a card
                (define new-card-list (deal 1))
                (set-player-hand! player (append new-card-list (player-hand player)))
                (when (eq? player you)
                  (send t flip-card (car new-card-list)))
                (rearrange-cards player)
                (if (check-hand player (car (player-hand player)))
                  ;; Drew a good card - keep going
                  (check-done
                   (lambda ()
                     (simulate-player player other-player #:other other-player-2 k)))
                  ;; End of our turn
                  (k))))))))))

;; Function to check for end-of-game
(: check-done (-> (-> Void) Void))
(define (check-done k)
  (if (ormap (lambda ([p : player]) (null? (player-hand p))) (list player-1 player-2 you))
    (begin (enable-your-cards #f)
           (send t set-status GAME-OVER-MESSAGE))
    (k)))

;; Look in opponents' initial hands for matches (Since each player gets 7
;; cards, it's impossible to run out of cards this way)
(: find-initial-matches (-> player Void))
(define (find-initial-matches player)
  (when (ormap (lambda ([card : (Instance Card%)]) (check-hand player card)) (player-hand player))
    ;; Found a match in the hand
    (find-initial-matches player)))
(find-initial-matches player-1)
(find-initial-matches player-2)
(find-initial-matches you)

;; Run the game loop
(let loop : Void () 
  (set-region-callback! (player-r you) (player-callback you (list-ref (list player-1 player-2) (random 2))))
  (set-region-callback! (player-r player-1) (player-callback player-1))
  (set-region-callback! (player-r player-2) (player-callback player-2))
  (send t set-status YOUR-TURN-MESSAGE)
  (yield something-happened)
  ;; player attempts to call fishing callback
  (let ([rcb (region-callback (player-r you))])
    (when rcb
      (rcb (player-hand you))))
  (if go-fish?
    (begin
      (if (if (null? deck)
            ;; No more cards; pass
            #f
            ;; Draw a card (wait for the user to drag it)
            (begin (send t set-status GO-FISH-MESSAGE)
                   (wiggle-top-card)
                   (enable-your-cards #f)
                   (set-region-callback! (player-r player-1) #f)
                   (set-region-callback! (player-r player-2) #f)
                   (set-region-callback! (player-r you) fishing)
                   (let ([rcb (region-callback (player-r you))])
                     (when rcb
                       (rcb null)))
                   ;(send (car deck) user-can-move #t)
                   (yield something-happened)
                   (enable-your-cards #t)
                   (check-hand you (car (player-hand you)))))
        (check-done loop)
        (begin (send t set-status GO-FISH-MESSAGE)
               (simulate-player
                you player-1 #:other player-2
                (lambda ()
                  (send t set-status PLAYER-1-NAME)
                  (simulate-player player-1 player-2
                                   (lambda ()
                                     (send t set-status PLAYER-2-NAME)
                                     (simulate-player player-2 player-1 loop))))))))
    (check-done loop)))
