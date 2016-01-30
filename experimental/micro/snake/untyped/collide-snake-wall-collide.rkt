#lang racket/base

(provide snake-wall-collide?)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
(only-in "collide-head-collide.rkt" head-collide?))

;; =============================================================================

;; Is the snake colliding with any of the walls?
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))
