#lang typed/racket/base

(provide World
         (struct-out world))

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt")
(require/typed/check "data-world.rkt"
  [#:struct world ([snake : Snake] [food : Posn])])

;; =============================================================================

(define-type World world)
