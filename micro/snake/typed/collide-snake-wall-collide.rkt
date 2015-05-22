#lang typed/racket/base

(provide snake-wall-collide?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-snake-adapted.rkt")
(require/typed/check "collide-head-collide.rkt"
  [head-collide? (-> Posn Boolean)])

;; =============================================================================

;; Is the snake colliding with any of the walls?
(: snake-wall-collide? : (Snake . -> . Boolean))
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))
