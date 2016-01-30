#lang typed/racket/base

(provide
  deal
  init-deck
  wiggle-top-card
)

;; -----------------------------------------------------------------------------

(require
  typed/racket/class
  "../base/games-cards-adapted.rkt"
)

;; =============================================================================

;; Set up the cards
(: init-deck (-> Natural Deck))
(define (init-deck N)
  (box (shuffle-list (make-deck) N)))

;; Function for dealing or drawing cards
(: deal (-> Deck Natural Card*))
(define (deal deck n)
  (let loop ([n : Natural n][d : Card* (unbox deck)])
    (if (zero? n)
      (begin (set-box! deck d) null)
      (cons (car d) (loop (sub1 n) (cdr d))))))

;; -----------------------------------------------------------------------------

;; Visual info to go fish
(: wiggle-top-card (-> Table Deck Void))
(define (wiggle-top-card t deck)
  (define top (car (unbox deck)))
  (define cw (send top card-width))
  (define ch (send top card-height))
  (let ([x (/ (- (send t table-width) cw) 2)]
        [y (- (/ (- (send t table-height) ch) 2) (/ ch 3))])
    (send t move-card top (- x 10) y)
    (send t move-card top (+ x 10) y)
    (send t move-card top x y)))

