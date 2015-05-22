#lang racket/base

(provide world->world)

;; -----------------------------------------------------------------------------

(require
         "data-world.rkt"
(only-in "motion-help-snake-grow.rkt" snake-grow)
(only-in "motion-help-snake-slither.rkt" snake-slither)
(only-in "motion-snake-eat.rkt" snake-eat)
(only-in "motion-eating.rkt" eating?))

;; =============================================================================

(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
