#lang racket/base

(provide game-over?)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-world.rkt"
(only-in "collide-snake-wall-collide.rkt" snake-wall-collide?)
(only-in "collide-snake-self-collide.rkt" snake-self-collide?))


;; =============================================================================

(define (game-over? w)
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))
