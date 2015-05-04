#lang typed/racket

(provide (struct-out block))

;; =============================================================================

(struct: block ([x : Real]
                [y : Real]
                [color : Symbol]))
