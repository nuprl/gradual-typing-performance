#lang typed/racket/base

;; a representation for game cards

(require "basics-types.rkt")
;(require/typed "basics.rkt"
;  )

(provide
 (struct-out card)
 
 ;; Card Card -> Boolean
 >-face
 
 ;; Card Card -> Face
 ;; assume for (--face c d) assume (>-face c d)
 --face)

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
