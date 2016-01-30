#lang racket

;; ---------------------------------------------------------------------------------------------------
;; interface specification for representing a game state, including internalization for players

(require "basics.rkt" "board.rkt" "Lib/auxiliaries.rkt" "Lib/contract.rkt" 2htdp/image)

(interface internal&
  ;; -------------------------------------------------------------------------------------------------
  (player? (-> any/c boolean?))
  
  (player-money    (-> player? cash?))
  (player-tiles    (-> player? (listof tile?)))
  (player-shares   (-> player? shares?))
  (player-external (-> player? any))
  (player-name     (-> player? string?))
  
  (player0
   ;; create an initial player from a name and six tiles 
   ;; the optional argument is a player 
   ;; (and/c (listof tile?) (λ (l) (= (length l) FULL-TILES)) distinct)
   (->* (string? tile? tile? tile? tile? tile? tile?) (any/c) player?))
  
  (xplayer?        (-> any/c any))
  (player->xexpr   (-> player? xplayer?))

  (*create-player 
   ;; create a mid-game player from tiles, shares, and cash, throw away the name 
   (-> string? cash? shares? (and/c [listof tile?] distinct (compose (<=/c STARTER-TILES#) length)) 
       player?))
  
  ;; ---------------------------------------------------------------------------------------------------
  ;; creating a game state, properties 
  
  (state?               (-> any/c boolean?))
  (state-hotels         (-> state? (listof hotel?)))
  (state-shares         (-> state? shares?))
  (state-sub-shares     (-> state? shares? state?))
  (state-tiles          (-> state? (listof tile?)))
  (state-board          (-> state? board?))
  (state-players        (-> state? (listof player?)))
  (state-current-player (-> state? player?))
  
  (state0
   ;; create the initial state from players with tile assignments
   ;; players are created with player0
   (->* () () #:rest (and/c (listof player?) (λ (l) (distinct (apply append (map player-tiles l)))))
        state?))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; game actions, from the administrator's perspective 
  
  (state-place-tile
   ;; place tile, account for effect of SINGLETON, GROWING, FOUNDING, MERGING
   (->i ((s state?) (t tile?)) #;"optional" ((h hotel?))
        #:pre/name (s t) "tile in posession of first player" 
        (member t (player-tiles (state-current-player s)))
        #:pre/name (s t) "tile is placable"
        (not (eq? (what-kind-of-spot (state-board s) t) IMPOSSIBLE))
        #:pre/name (s t h) "if h is given, you better have a founding and or a merging spot"
        (or (unsupplied-arg? h) (memq (what-kind-of-spot (state-board s) t) (list FOUNDING MERGING)))
        ;; if spot is FOUNDING and hotels are available, h must be one of them 
        #:pre/name (s t h) "FOUNDING must happen when possible"
        (let ((b (state-board s))
              (hotels (state-hotels s)))
          (==> (and (eq? (what-kind-of-spot b t) FOUNDING) (cons? hotels)) 
               (and (not (unsupplied-arg? h)) (member h hotels))))
        #:pre/name (s t h) "if tile placement causes merger, hotel must be given and an acquirer"
        (==> (eq? (what-kind-of-spot (state-board s) t) MERGING) 
             (and (not (unsupplied-arg? h)) 
                  (let-values ([(w _) (merging-which (state-board s) t)]) (member h w))))

        ;; if spot is MERGING, the hotel should be a "large" one
        (next-state state?)))
  
  (state-buy-shares
   ;; banker sells shares to current player 
   (->i ((s state?) (shares shares-order/c))
        #:pre/name (s shares) "player has enough money" 
        (affordable? (state-board s) shares (player-money (state-current-player s)))
        #:pre/name (s shares) "shares are available"
        (let ([banker-s-shares (state-shares s)])
          (shares-available? banker-s-shares shares))
        (next-state state?)))
  
  (state-return-shares 
   ;; player p returns shares for acquired hotels 
   (->i ((s state?) (d (listof (list/c player? (listof (list/c hotel? boolean?))))))
        ;; if optional board is specified, use the hotel sizes from there
        ((b board?))
        #:pre (s d) (= (length (state-players s)) (length d))
        (next state?)))
  
  (state-move-tile
   ;; move the tile from the banker's pile to the current player's pile
   (->i ((s state?) (t tile?)) 
        #:pre/name (s t) "banker's tile?" (member t (state-tiles s))
        (next-state state?)))
  
  (state-next-turn 
   ;; switch turns of player
   (-> state? state?))
  
  (state-remove-current-player
   ;; remove the current player 
   (-> state? state?))
  
  (state-eliminate
   ;; remove the list of specified players
   (-> state? (listof player?) state?))
  
  (state-score
   ;; score the current state 
   (-> state? score/c))
  
  (state-final?
   (-> state? boolean?))
  
  (state-draw
   (-> state? image?))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; externalizing states
  (xstate?       (-> any/c boolean?))
  (state->xexpr  (-> state? xstate?))
  [*create-state (->i ((b board?)
		       (lp
			 (let ()
			   ;; [Listof Player] -> Boolean 
			   (define (combinable? lp)
			     (define d (shares-combinable? (map player-shares lp)))
			     (unless d
			       (displayln `(shares not combinable ,(map player-shares lp))))
			     d)
			   (and/c (listof player?) combinable?))))
                      #:pre/name (b lp) "players' tiles and tiles on board are mutually distinct"
                      (distinct (apply append (board-tiles b) (map player-tiles lp)))
                      (result state?))]
  [*cs0 (->* () #:rest (and/c (listof (and/c string? (λ (n) (<= (string-length n) 20)))) distinct)
             state?)])
  
(provide score/c)

(define score/c (and/c (listof (list/c string? cash?)) (sorted >= #:key second)))
