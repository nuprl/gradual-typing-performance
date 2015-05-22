#lang racket/base

(provide snake-eat)

;; -----------------------------------------------------------------------------

(require
         "data-posn.rkt"
         "data-world.rkt"
(only-in "const-board-width.rkt" BOARD-WIDTH)
(only-in "const-board-height.rkt" BOARD-HEIGHT)
(only-in "motion-r.rkt" r)
(only-in "motion-help-snake-grow.rkt" snake-grow))

;; =============================================================================

;; Eat the food and generate a new one.
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)))
