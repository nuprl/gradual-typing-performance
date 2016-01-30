#lang typed/racket/base

(provide Dir
         Snake
         (struct-out snake))

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt")

;; =============================================================================

(define-type Dir (U "up" "down" "left" "right"))

(define-type Snake snake)

(require/typed/check "data-snake.rkt"
  [#:struct snake
            ([dir : Dir]
             [segs : NEList])])
