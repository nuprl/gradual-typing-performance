#lang typed/racket

(provide (struct-out posn))

;; =============================================================================

(struct: posn ([x : Real]
               [y : Real]))
