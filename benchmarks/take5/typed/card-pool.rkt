#lang typed/racket/base

;; a representation of the complete deck of cards 

(provide
 ;;   { [Listof X] -> [Listof X] }
 ;;   { -> Bulls }
 ;; -> CardPool
 ;; create and organize the pool of cards in a random order so that
 ;; the dealer can hand cards to players and create the initial deck
 ;;
 ;; so we can keep things deterministic 
 ;; the first optional argument is a shuffle algorithm for lists
 ;; the second optional argument generates bulls 
 create-card-pool)

;; -----------------------------------------------------------------------------

(require
  "card-adapted.rkt"
  "basics-types.rkt"
  "card-pool-types.rkt"
  typed/racket/class
  require-typed-check
  (only-in racket/list shuffle first rest))

(require/typed/check "basics.rkt"
  (FACE  Natural)
  (HAND  Natural)
  (MIN-BULL Natural)
  (MAX-BULL Natural))

(require/typed racket/base
  (random (-> Integer Integer Integer)))

;; ---------------------------------------------------------------------------------------------------
(: create-card-pool (->* () ((-> (Listof Card) (Listof Card)) (-> Bulls)) CardPool))
(define (create-card-pool (shuffle shuffle) (random-bulls random-bulls))
  (new card-pool% (shuffle shuffle) (random-bulls random-bulls)))

;; -> Bulls
;; pick a random number of BULLS 
(: random-bulls (-> Bulls))
(define (random-bulls)
  (cast (random MIN-BULL (+ MAX-BULL 1)) Bulls))

(: card-pool% CardPool%)
(define card-pool%
  (class object%
    (init-field (shuffle shuffle) (random-bulls random-bulls))
    (super-new)

    (define my-cards : (Listof Card)
      (shuffle (build-list FACE (lambda ([i : Natural]) (card (cast (+ i 1) Face) (random-bulls))))))

    (define/public (draw-card)
      (begin0 (first my-cards)
              (set! my-cards (rest my-cards))))

    (define/public (draw-hand)
      (cast (build-list HAND (lambda (_) (draw-card))) Hand))))
