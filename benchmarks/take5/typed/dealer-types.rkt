#lang typed/racket/base

(require
  "basics-types.rkt"
  typed/racket/class)

(define-type Result (Listof (List Name Bulls)))

(define-type Dealer-Internal

(define-type Dealer%
  (Class
    (init-field (players (Listof Player)))
    (bulls (-> Natural))
    (play-game (-> Result))))
(define-type Dealer (Instance Dealer%))

