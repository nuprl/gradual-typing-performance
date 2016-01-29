#lang racket/base

(provide world-change-dir)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-world.rkt"
(only-in "motion-snake-change-direction.rkt" snake-change-direction))

;; =============================================================================

;; Change direction of the world.
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))
