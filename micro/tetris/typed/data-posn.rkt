#lang typed/racket/base

(provide (struct-out posn))

;; =============================================================================

(struct: posn ([x : Real]
               [y : Real]))
