#lang typed/racket/base

(provide try-to-set-suffix-edge!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

;; try-to-set-suffix-edge!: node node -> void
;;
;; Sets the suffix edge of from-node directed to to-node if it
;; hasn't been set yet.
(: try-to-set-suffix-edge! (-> Node Node Void))
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
    (set-node-suffix-link! from-node to-node)))
