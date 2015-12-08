#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for internal game states 
;; -- inspecting the game state 
;; -- manipulating the game state 
;; including a data structure for internalizing the state of the players

(require
 "board-adapted.rkt"
 "../base/types.rkt"
 typed/racket/class)

(define-type State state)
(require/typed "state.rkt"
  (score? (-> Any Boolean))
  (#:opaque Player player?)
  (player-name (-> Player String))
  (player-tiles (-> Player (Listof Tile)))
  (player-money (-> Player Cash))
  (player-shares (-> Player Shares))
  (player-external (-> Player (Option (Instance Player%))))
  ;; (#:opaque State state?)
  ;; (state-board (-> State Board))
  ;; (state-players (-> State (Listof Player)))
  ;; (state-tiles (-> State (Listof Tile)))
  ;; (state-hotels (-> State (Listof Hotel)))
  ;; (state-shares (-> State Shares))
  (#:struct state
   ([board : Board]
    [players : (Listof Player)]
    [tiles : (Listof Tile)]
    [hotels : (Listof Hotel)]
    [shares : Shares]
    [bad : (Listof Player)]))
  ;; --
  (*create-player (-> String Cash Shares (Listof Tile) Player))
  (player0 (-> String Tile Tile Tile Tile Tile Tile (Instance Player%) Player))
                   ;; (-> String Tile Tile Tile Tile Tile Tile Player)
                   ;; (-> String Tile * Player)))
  (state0 (-> Player * State))
  (state-sub-shares (-> State Shares State))
  (*cs0 (-> String * State))
  (*create-state (-> Board (Listof Player) State))
  (state-place-tile (->* (State Tile) ((Option Hotel)) State))
  (state-move-tile (-> State Tile State))
  (state-next-turn (-> State State))
  (state-remove-current-player (-> State State))
  (state-eliminate (-> State (Listof Player) State))
  (state-current-player (-> State Player))
  (state-buy-shares (-> State (Listof Hotel) State))
  (state-return-shares (->* [State Decisions] [Board] State))
  (state-score (-> State (Listof (List String Cash))))
  (state-final? (-> State Boolean))
)

;; -----------------------------------------------------------------------------

(define-type Decisions (Listof (List Player (Listof (List Hotel Boolean)))))
(define-type Score (Listof (List String Cash)))

(define-type Administrator%
  (Class
   (init-field
    (next-tile (-> (Listof Tile) Tile)))
   (sign-up (-> String (Instance Player%) String))
   (show-players (-> (Listof String)))
   (run (->* (Natural) (#:show (-> Void)) RunResult))
   ))
(define-type Administrator (Instance Administrator%))

(define-type Turn%
  (Class
   (init-field (current-state State))
   (field
    (board Board)
    (current Player)
    (cash Cash)
    (tiles (Listof Tile))
    (shares Shares)
    (hotels (Listof Hotel))
    (players (Listof Player)))
   (reconcile-shares (-> Shares Shares))
   (eliminated (-> (Listof Player)))
   (place-called (-> Boolean))
   (decisions (-> (Values (Option Tile) (Option Hotel) Decisions)))
   ;; Precondition: (send this place-called)
   (place (-> Tile Hotel (U Void (Listof Player))))
   ))
(define-type Turn (Instance Turn%))

(define-type Player%
  (Class
   (init-field
    [name String]
    [choice Strategy])
   (field
    [*players (Listof Player)]
    [*bad (Listof Player)])
   (go (-> Administrator Void))
   (setup (-> State Void))
   (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (keep (-> (Listof Hotel) (Listof Boolean)))
   (receive-tile (-> Tile Void))
   (inform (-> State Void))
   (the-end (-> State Any Void))))
           
(define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
(define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))

;; -----------------------------------------------------------------------------

(provide
 player?
 player-money
 player-tiles
 player-shares
 player-external
 player-name
 player0
 *create-player
 state?
 state-hotels
 state-shares
 state-sub-shares
 state-tiles
 state-board
 state-players
 state-current-player
 state0
 state-place-tile
 state-buy-shares
 state-return-shares
 state-move-tile
 state-next-turn
 state-remove-current-player
 state-eliminate
 state-score
 state-final?
 *create-state
 *cs0
 ;; --
 Score
 Player
 State
 Decisions
 ;; --
 Administrator%
 Administrator
 Turn%
 Turn
 Player%
 RunResult
 Strategy
 )

