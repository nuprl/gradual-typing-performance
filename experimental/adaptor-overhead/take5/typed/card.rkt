#lang typed/racket/base

;; a representation for game cards

(provide
 (struct-out card)

 ;; Card Card -> Boolean
 >-face

 ;; Card Card -> Face
 ;; assume for (--face c d) assume (>-face c d)
 --face)

(require "basics-types.rkt")

;; ---------------------------------------------------------------------------------------------------

;; -- card.rkt
(struct card (
 [face : Face]
 [bulls : Bulls])
#:transparent)
(define-type Card card)

(: >-face (-> Card Card Boolean))
(define (>-face c d)
  (> (card-face c) (card-face d)))

(: --face (-> Card Card Face))
(define (--face c d)
  (cast (- (card-face c) (card-face d)) Face))
