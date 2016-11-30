#lang racket/base

(module guess-game racket/base
  (provide play)

  (define (play)
    (define n (random 10))
    (λ (guess)
      (= guess n)))
)

(module player racket/base
  (provide stubborn-player)

  (define (stubborn-player n)
    4)
)

(module go typed/racket/base
  (require/typed (submod ".." guess-game)
    [play (-> (-> Natural Boolean))])

  (require/typed (submod ".." player)
    (stubborn-player (-> Natural Natural)))

  (define check-guess (play))

  (for/sum ([i 5])
    (if (check-guess (stubborn-player i)) 1 0))
)

(require 'go)
