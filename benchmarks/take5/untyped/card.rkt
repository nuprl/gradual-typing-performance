#lang racket 

;; a representation for game cards

(require "basics.rkt" "../base/utility.rkt")

(types Face Bulls)

(provide
 (type Card)
 
 ;; Face Bulls -> Card 
 card
 
 ;; Card -> Face 
 card-face
 
 ;; Card -> Bull
 card-bulls
 
 ;; Card Card -> Boolean
 >-face
 
 ;; Card Card -> Face
 ;; assume for (--face c d) assume (>-face c d)
 --face)

;; ---------------------------------------------------------------------------------------------------

(struct card (face bulls) #:transparent)
(define-type Card (card Face Bull))

(define (>-face c d) (> (card-face c) (card-face d)))

(define (--face c d) (- (card-face c) (card-face d)))
