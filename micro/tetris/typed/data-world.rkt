#lang typed/racket/base

(provide (struct-out world))

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-block-adapted.rkt")

;; =============================================================================

(struct: world ([tetra : tetra]
                [blocks : (Listof block)]))
