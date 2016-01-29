#lang typed/racket/base

(provide BOARD-WIDTH-PIXELS)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "const-board-width.rkt"
  [BOARD-WIDTH Natural])
(require/typed/check "const-grid-size.rkt"
  [GRID-SIZE Natural])

;; =============================================================================

(: BOARD-WIDTH-PIXELS Natural)
(define BOARD-WIDTH-PIXELS (* GRID-SIZE BOARD-WIDTH))
