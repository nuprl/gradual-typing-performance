#lang racket/base

(provide (struct-out world))

(require benchmark-util
         "data-tetra.rkt"
         "data-block.rkt")

;; =============================================================================

(struct world (tetra
                blocks))
