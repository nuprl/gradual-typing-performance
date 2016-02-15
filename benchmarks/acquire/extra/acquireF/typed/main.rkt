#lang typed/racket/base
(random-seed 5)

;; This is `tree-game.rkt`

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/list
  typed/racket/class
  "board-adapted.rkt"
  "state-adapted.rkt"
  )
(require/typed/check "admin.rkt"
  (administrator% Administrator%)
  )
(require/typed/check "player-factory.rkt"
 (random-players (-> Natural (Listof (Instance Player%))))
 (inf-loop-player (-> Natural (Instance Player%)))
)
(require/typed/check "auxiliaries.rkt"
  (randomly-pick (-> (Listof Tile) Tile))
)

;; =============================================================================

(: go (-> (Instance Player%) Void))
(define (go extra)
  (define p1 (random-players 5))
  (define p (cons extra p1))
  (define-values (two-status _score two-run)
    (let ([r (run p 99 #:show show #:choice randomly-pick)])
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
    (go (inf-loop-player 0))))

(time (main 1))
