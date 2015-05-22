#lang typed/racket/base

(provide (struct-out world))

;; -----------------------------------------------------------------------------

(require "data-snake-adapted.rkt"
         "data-posn-adapted.rkt")

;; =============================================================================

(struct world ([snake : Snake]
               [food  : Posn]))
