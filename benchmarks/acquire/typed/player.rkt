#lang typed/racket

;; ---------------------------------------------------------------------------------------------------
;; Acquire player that manages all the interactions; delegates decision making to separate strategies 

(provide
 random-players
 ordered-players
 inf-loop-player
 )

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION 

(require
 require-typed-check
 "../base/types.rkt"
 "state-adapted.rkt"
)
(require/typed/check "admin.rkt"
  (administrator% Administrator%)
  (turn% Turn%)
)
(require/typed/check "basics.rkt"
  (player-shares0 Shares)
  (*combine-shares (-> (Listof Shares) Shares))
  (shares-minus (-> Shares Shares Shares))
  (banker-shares0 Shares)
)
(require/typed/check "strategy.rkt"
  (ordered-s Strategy)
  (random-s Strategy)
)

;; -----------------------------------------------------------------------------

;; (define (player? x)
;;   (is-a? x player%))

(: create (-> String Strategy (Instance Player%)))
(define (create n c)
  (new player% [name n][choice c]))

(: player% Player%)
(define player%
  (class object%
    (init-field name choice)
    (super-new)
    
    (field [*players '()]
           [*bad '()])

    (: *my-game-name String)
    (define *my-game-name "")
    
    (define/public (go administrator)
      (set! *my-game-name (send administrator sign-up name this)))
    
    (define/public (setup s)
      (bad-players (state-players s)))
    
    (define/public (take-turn turn) 
      (bad-players (get-field players turn))
      ;; reconcile is added for remote players that don't get complete information
      (choice (reconcile turn)))
      ;; (define-values (to-place hotel shares-to-buy) 
      ;; (values to-place hotel shares-to-buy))
    
    (define/public (keep hotels)
      (map (lambda (h) (< (random 100) 50)) hotels))
    
    (define/public (receive-tile t)
      (void))
    
    (define/public (inform s)
      (bad-players (state-players s)))
    
    (define/public (the-end state results)
      ;; I should figure out what to do here 
      (void))
    
    ;; [Listof Players] -> Void
    ;; given the list of current players, find out which ones dropped out since the last update
    ;; effect: move the drop-out players to this.*bad, keep the good ones in this.*players
    (: bad-players (-> (Listof Player) Void))
    (define/private (bad-players players)
      (set! *bad 
            (for/fold : (Listof Player)
                      ([bad : (Listof Player) *bad])
                      ((old-player : Player (in-list *players))) 
              (define n (player-name old-player))
              (if (findf (lambda ([current : Player])
                           (string=? (player-name current) n)) players)
                  bad
                  (cons old-player bad))))
      (set! *players players))
    
    ;; Turn -> Turn 
    ;; effect: reduce the turn's banker-shares by the shares of this.*bad players
    (: reconcile (-> (Instance Turn%) (Instance Turn%)))
    (define/private (reconcile turn)
      (define bad-shares (*combine-shares (map player-shares *bad)))
      (send turn reconcile-shares bad-shares)
      turn)))

;; -----------------------------------------------------------------------------
;; from `player-factory.rkt`

(: players (-> Strategy Natural (Listof (Instance Player%))))
(define (players S n)
  (for/list ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create name S)))

(: random-players (-> Natural (Listof (Instance Player%))))
(define (random-players n)
  (players random-s n))

(: ordered-players (-> Natural (Listof (Instance Player%))))
(define (ordered-players n)
  (players ordered-s n))

(: inf-loop-player (-> Natural (Instance Player%)))
(define (inf-loop-player n)
  (define m 0)
  (: S Strategy)
  (define (S t)
    (if (> n m)
        (begin (set! m (+ m 1)) (ordered-s t))
        (let L () (L))))
  (create (format "inf loop after ~a" n) S))

