#lang typed/racket/base

;; the main entry point for an OO implementation of "6 Nimmt!" (Take 5)

(provide
 main)

(require
  benchmark-util
  typed/racket/class
  "player-types.rkt"
  "dealer-types.rkt"
)

(require/typed/check "player.rkt"
  (create-player (-> Natural Player)))

(require/typed/check "dealer.rkt"
  (create-dealer (-> (Listof Player) Dealer))
)

;; =============================================================================

(: main (-> (U String Natural) Result))
(define (main n)
  (define k
    (cond
      [(and (string? n) (string->number n))
       => (lambda (x) (assert n index?))]
      [(index? n) n]
      [else
       (error 'main "input must be a natural number; given ~e" n)]))
  (define players (build-list k create-player))
  (define dealer (create-dealer players))
  (send dealer play-game))

(define PLAYERS 10)
(define ITERS PLAYERS)

(module+ test
  (unless (equal? (main PLAYERS)
                  '((after-round 2)
                    ((1 0) (2 0) (3 0) (6 0) (7 0) (8 0) (0 56) (4 80) (9 80) (5 120))))
    (raise-user-error 'take5 "TEST FAILURE")))

(module+ main
  (time (for ([n (in-range ITERS)]) (main PLAYERS))))
