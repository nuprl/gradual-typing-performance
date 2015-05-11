#lang typed/racket/base

(provide game-over?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "collide-snake-wall-collide.rkt"
  [snake-wall-collide? (Snake . -> . Boolean)])
(require/typed/check "collide-snake-self-collide.rkt"
  [snake-self-collide? (Snake . -> . Boolean)])


;; =============================================================================

(: game-over? : (World . -> . Boolean))
(define (game-over? w)
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))
