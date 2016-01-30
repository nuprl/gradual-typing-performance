#lang typed/racket/base

(provide snake-eat)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "const-board-width.rkt"
  [BOARD-WIDTH Natural])
(require/typed/check "const-board-height.rkt"
  [BOARD-HEIGHT Natural])
(require/typed/check "motion-r.rkt"
  [r Pseudo-Random-Generator])
(require/typed/check "motion-help-snake-grow.rkt"
  [snake-grow (-> Snake Snake)])

;; =============================================================================

;; Eat the food and generate a new one.
(: snake-eat : (World . -> . World))
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)))
