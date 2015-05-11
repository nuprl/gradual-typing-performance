#lang typed/racket/base

(provide eating?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-posn-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "data-posn-eq.rkt"
  [posn=? (-> Posn Posn Boolean)])

;; =============================================================================

;; Is the snake eating the food in the world.
(: eating? : (World . -> . Boolean))
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
