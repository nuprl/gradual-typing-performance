#lang typed/racket/base

;; the main entry point for an OO implementation of "6 Nimmt!" (Take 5)

(provide
 ;; (U String N) -> [Listof [List N N]]
 ;; (main n) creates n players, hands them to the dealer, and asks the
 ;; latter to run a complete simulation of a 6-Nimmit! game
 ;; EFFECT also write result to stdout
 ;; NOTE the default player and dealer are completely deterministic
 ;; To run a random simulation, it is necessary to override the defaults
 main)

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------

;; -- player.rkt
(require
  (only-in racket/contract natural-number/c)
)

(require/typed "player.rkt"
  ())

(require/typed "dealer.rkt"
  (create-dealer (-> (Listof Player) Dealer))

(define (main n)
  (define (e)
    (error 'main "input must be a natural number; given ~e" n))
  (define k
    (cond
      [(string? n)
       (define l (string->number n))
       (if (natural-number/c l) l (e))]
      [(natural-number/c n) n]
      [else (e)]))
  (define players (build-list k create-player))
  (define dealer (create-dealer players))
  (send dealer play-game))

(time
  (void
   (with-output-to-string
      (lambda () (main 10)))))

