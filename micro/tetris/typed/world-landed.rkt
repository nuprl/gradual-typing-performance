#lang typed/racket/base

(provide landed?)

(require benchmark-util
         "data-world-adapted.rkt")
(require/typed/check "world-landed-on-blocks.rkt"
  [landed-on-blocks? (-> World Boolean)])
(require/typed/check "world-landed-on-floor.rkt"
  [landed-on-floor? (-> World Boolean)])

;; =============================================================================

;; Has the current tetra landed?
(: landed? (-> World Boolean))
(define (landed? w)
  (or (landed-on-blocks? w)
      (landed-on-floor? w)))
