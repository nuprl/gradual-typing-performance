#lang typed/racket/base

(provide world-key-move)

(require benchmark-util
         "data-world-adapted.rkt")
(require/typed/check "world-world-move.rkt"
  [world-move (-> Real Real World World)])
(require/typed/check "world-world-jump-down.rkt"
  [world-jump-down (-> World World)])
(require/typed/check "world-world-rotate-ccw.rkt"
  [world-rotate-ccw (-> World World)])
(require/typed/check "world-world-rotate-cw.rkt"
  [world-rotate-cw (-> World World)])

;; =============================================================================

;; Move the world according to the given key event.
(: world-key-move (-> World String World))
(define (world-key-move w k)
  (cond [(equal? k "left") (world-move -1 0 w)]
        [(equal? k "right") (world-move 1 0 w)]
        [(equal? k "down") (world-jump-down w)]
        [(equal? k "a") (world-rotate-ccw w)]
        [(equal? k "s") (world-rotate-cw w)]
        [else w]))
