#lang racket/base

(provide head-collide?)

;; -----------------------------------------------------------------------------

(require
         "data-posn.rkt"
(only-in "const-board-width.rkt" BOARD-WIDTH)
(only-in "const-board-height.rkt" BOARD-HEIGHT))

;; =============================================================================

(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))
