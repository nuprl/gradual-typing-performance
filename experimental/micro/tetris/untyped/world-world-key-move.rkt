#lang racket/base

(provide world-key-move)

(require benchmark-util
         "data-world.rkt"
 "world-world-move.rkt"
 "world-world-jump-down.rkt"
 "world-world-rotate-ccw.rkt"
 "world-world-rotate-cw.rkt")

;; =============================================================================

;; Move the world according to the given key event.
;(: world-key-move (-> World String World))
(define (world-key-move w k)
  (cond [(equal? k "left") (world-move -1 0 w)]
        [(equal? k "right") (world-move 1 0 w)]
        [(equal? k "down") (world-jump-down w)]
        [(equal? k "a") (world-rotate-ccw w)]
        [(equal? k "s") (world-rotate-cw w)]
        [else w]))
