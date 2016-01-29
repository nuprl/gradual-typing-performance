#lang racket/base

(provide snake-change-direction)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt")

;; =============================================================================

;; Change the direction of the snake.
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
