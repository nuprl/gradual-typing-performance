#lang typed/racket/base

(provide world-change-dir)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "motion-snake-change-direction.rkt"
  [snake-change-direction (-> Snake Dir Snake)])

;; =============================================================================

;; Change direction of the world.
(: world-change-dir (-> World Dir World))
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))
