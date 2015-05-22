#lang typed/racket/base

(provide list-pick-random)

(require "data-tetra-adapted.rkt")

;; =============================================================================

(define r (make-pseudo-random-generator))
(parameterize ((current-pseudo-random-generator r))
  (random-seed 43453))

(: list-pick-random (-> (Listof Tetra) Tetra))
(define (list-pick-random ls)
  (list-ref ls (random (length ls) r)))
