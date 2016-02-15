#lang typed/racket/base

(provide
  random-players
  inf-loop-player
)

(require
 benchmark-util
 "../base/types.rkt"
 "state-adapted.rkt"
)
(require/typed/check "strategy.rkt"
  (ordered-s Strategy)
  (random-s Strategy)
)
(require/typed/check "player.rkt"
  (create (-> String Strategy (Instance Player%)))
)

;; -----------------------------------------------------------------------------
;; from `player-factory.rkt`

(: players (-> Strategy Natural (Listof (Instance Player%))))
(define (players S n)
  (for/list ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create name S)))

(: random-players (-> Natural (Listof (Instance Player%))))
(define (random-players n)
  (players random-s n))

(: inf-loop-player (-> Natural (Instance Player%)))
(define (inf-loop-player n)
  (define m 0)
  (: S Strategy)
  (define (S t)
    (if (> n m)
        (begin (set! m (+ m 1)) (ordered-s t))
        (let L () (L))))
  (create (format "inf loop after ~a" n) S))

