#lang racket/base

(provide snake-self-collide?)

;; -----------------------------------------------------------------------------

(require
         "data-snake.rkt"
         "data-posn.rkt"
(only-in "collide-segs-self-collide.rkt" segs-self-collide?))

;; =============================================================================

(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))
