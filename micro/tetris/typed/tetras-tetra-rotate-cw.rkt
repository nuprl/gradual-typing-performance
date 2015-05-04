#lang typed/racket/base

(provide tetra-rotate-cw)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "bset-blocks-rotate-cw.rkt"
  [blocks-rotate-cw (-> Posn BSet BSet)])

;; =============================================================================

;; Rotate the tetra 90 degrees clockwise around its center.
(: tetra-rotate-cw (-> Tetra Tetra))
(define (tetra-rotate-cw t)
  (tetra (tetra-center t)
         (blocks-rotate-cw (tetra-center t)
                           (tetra-blocks t))))
