#lang typed/racket/base

(provide BOARD-HEIGHT-PIXELS)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "const-board-height.rkt"
  [BOARD-HEIGHT Natural])
(require/typed/check "const-grid-size.rkt"
  [GRID-SIZE Natural])

;; =============================================================================

(: BOARD-HEIGHT-PIXELS Natural)
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT))
