#lang racket/base

(provide
  random-players
  inf-loop-player
)

(require
 "state.rkt"
)
(require (only-in "strategy.rkt"
  ordered-s
  random-s
))
(require (only-in "player.rkt"
  create
))

;; -----------------------------------------------------------------------------
;; from `player-factory.rkt`

(define (players S n)
  (for/list ((name '("a" "b" "c" "d" "e" "f")) (i (in-range n))) (create name S)))

(define (random-players n)
  (players random-s n))

(define (inf-loop-player n)
  (define m 0)
  (define (S t)
    (if (> n m)
        (begin (set! m (+ m 1)) (ordered-s t))
        (let L () (L))))
  (create (format "inf loop after ~a" n) S))

