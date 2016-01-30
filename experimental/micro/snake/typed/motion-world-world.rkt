#lang typed/racket/base

(provide world->world)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-snake-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "motion-help-snake-grow.rkt"
  [snake-grow (-> Snake Snake)])
(require/typed/check "motion-help-snake-slither.rkt"
  [snake-slither (-> Snake Snake)])
(require/typed/check "motion-snake-eat.rkt"
  [snake-eat (-> World World)])
(require/typed/check "motion-eating.rkt"
  [eating? (-> World Boolean)])

;; =============================================================================

(: world->world : (World . -> . World))
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
