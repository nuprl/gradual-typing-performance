#lang typed/racket/base

(provide r)

;; -----------------------------------------------------------------------------
;; =============================================================================

(: r Pseudo-Random-Generator)
(define r (make-pseudo-random-generator))
