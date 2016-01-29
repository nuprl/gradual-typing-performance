#lang typed/racket/base

(provide snake-change-direction)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt")

;; =============================================================================

;; Change the direction of the snake.
(: snake-change-direction : (Snake Dir . -> . Snake))
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
