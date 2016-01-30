#lang racket/base

(provide landed?)

(require benchmark-util
         "data-world.rkt")
(require "world-landed-on-blocks.rkt")
(require "world-landed-on-floor.rkt")

;; =============================================================================

;; Has the current tetra landed?
;(: landed? (-> World Boolean))
(define (landed? w)
  (or (landed-on-blocks? w)
      (landed-on-floor? w)))
