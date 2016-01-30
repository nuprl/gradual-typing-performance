#lang typed/racket

(require "make-cards.rkt" "classes.rkt" "card-class.rkt" "typed-base.rkt")

(define make-table
  (lambda ([title : String "Cards"][w : Nonnegative-Real 7][h : Nonnegative-Real 3])
    (make-object table% title w h)))

(: make-deck (-> (Listof (Instance Card%))))
(define (make-deck)
  ((inst map (Instance Card%) (Instance Card%)) (lambda ([l : (Instance Card%)]) (send l copy)) deck-of-cards))