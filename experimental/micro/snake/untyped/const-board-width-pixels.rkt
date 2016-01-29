#lang racket/base

(provide BOARD-WIDTH-PIXELS)

;; -----------------------------------------------------------------------------

(require
(only-in"const-board-width.rkt" BOARD-WIDTH)
(only-in "const-grid-size.rkt" GRID-SIZE))

;; =============================================================================

(define BOARD-WIDTH-PIXELS (* GRID-SIZE BOARD-WIDTH))
