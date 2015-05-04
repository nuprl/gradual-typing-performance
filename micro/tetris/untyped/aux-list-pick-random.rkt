#lang racket/base

(provide list-pick-random)

(require "data-tetra.rkt")

;; =============================================================================

(define r (make-pseudo-random-generator))
(parameterize ((current-pseudo-random-generator r))
  (random-seed 43453))

(define (list-pick-random ls)
  (list-ref ls (random (length ls) r)))
