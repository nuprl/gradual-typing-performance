#lang typed/racket/base

(provide (struct-out suffix-tree)
         Tree)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         benchmark-util)
(require/typed/check "data-suffix-tree.rkt"
  [#:struct suffix-tree ([root : node])])

;; =============================================================================

(define-type Tree suffix-tree)
