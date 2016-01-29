#lang typed/racket/base

(provide head-collide?)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-posn-adapted.rkt")
(require/typed/check "const-board-width.rkt"
  [BOARD-WIDTH Natural])
(require/typed/check "const-board-height.rkt"
  [BOARD-HEIGHT Natural])

;; =============================================================================

(: head-collide? : (Posn . -> . Boolean))
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))
