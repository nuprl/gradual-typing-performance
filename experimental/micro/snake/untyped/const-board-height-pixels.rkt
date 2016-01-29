#lang racket/base

(provide BOARD-HEIGHT-PIXELS)

;; -----------------------------------------------------------------------------

(require
(only-in "const-board-height.rkt" BOARD-HEIGHT)
(only-in "const-grid-size.rkt" GRID-SIZE))

;; =============================================================================

(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT))
