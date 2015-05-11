#lang racket/base

(provide eating?)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-world.rkt"
(only-in "data-posn-eq.rkt" posn=?))

;; =============================================================================

;; Is the snake eating the food in the world.
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
