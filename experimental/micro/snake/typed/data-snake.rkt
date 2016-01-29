#lang typed/racket/base

(provide (struct-out snake))

;; -----------------------------------------------------------------------------

(require "data-posn-adapted.rkt")

;; =============================================================================

(struct snake ([dir  : (U "up" "down" "left" "right")]
               [segs : NEList]))
