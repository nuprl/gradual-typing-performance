#lang typed/racket/base
(random-seed 5)

;; This is `tree-game.rkt`

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/list
  typed/racket/class
  "../base/types.rkt"
  "board.rkt"
  "state.rkt"
  )
(require/typed/check "admin.rkt"
  (administrator% Administrator%)
  )
(require/typed/check "player.rkt"
 (random-players (-> Natural (Listof (Instance Player%))))
 (ordered-players (-> Natural (Listof (Instance Player%))))
 (inf-loop-player (-> Natural (Instance Player%)))
)
(require/typed/check "auxiliaries.rkt"
  (randomly-pick (-> (Listof Tile) Tile))
)

(define-type Board (HashTable Tile Content))
(define-type Tile tile)
(define-type State state)
(define-type Player player)
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

(define-type Player%
  (Class
   (init-field
    [name String]
    [choice Strategy])
   (field
    [*players (Listof Player)]
    [*bad (Listof Player)])
   (go (-> (Instance Administrator%) Void))
   (setup (-> State Void))
   (take-turn (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
   (keep (-> (Listof Hotel) (Listof Boolean)))
   (receive-tile (-> Tile Void))
   (inform (-> State Void))
   (the-end (-> State Any Void))))
(define-type RunResult (List (U 'done 'exhausted 'score 'IMPOSSIBLE) Any (Listof State)))
(define-type Strategy (-> (Instance Turn%) (Values (Option Tile) (Option Hotel) (Listof Hotel))))
;; =============================================================================

(: go (-> (Instance Player%) Void))
(define (go extra)
  (define p0 (ordered-players 10))
  (define p1 (random-players 10))
  (define p (cons extra (append p0 p1)))
  (define-values (two-status _score two-run)
    (let ([r (run p 10 #:show show #:choice randomly-pick)])
      (values (car r) (cadr r) (caddr r))))
  ;(displayln `(,(length two-run) ,two-status))
  (void))

(: run (->* ((Listof (Instance Player%)) Natural)
            (#:show (-> Void) #:choice (-> (Listof Tile) Tile))
            RunResult))
(define (run players turns# #:show (show show) #:choice (choose-next-tile first))
  (define a (new administrator% (next-tile choose-next-tile)))
  (for ((p players)) (send p go a))
  (send a run turns# #:show show))

;; -> (Nat Board -> Void)
(: show (-> Void))
(define (show)
  (void))

(: main (-> Natural Void))
(define (main n)
  (for ((i (in-range n)))
    (go (inf-loop-player 99))))

(time (main 10))
