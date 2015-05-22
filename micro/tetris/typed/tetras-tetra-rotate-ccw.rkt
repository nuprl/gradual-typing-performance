#lang typed/racket/base

(provide tetra-rotate-ccw)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "bset-blocks-rotate-ccw.rkt"
  [blocks-rotate-ccw (-> Posn BSet BSet)])

;; =============================================================================

;; Rotate the tetra 90 degrees counterclockwise around its center.
(: tetra-rotate-ccw (-> Tetra Tetra))
(define (tetra-rotate-ccw t)
  (tetra (tetra-center t)
         (blocks-rotate-ccw (tetra-center t)
                            (tetra-blocks t))))
