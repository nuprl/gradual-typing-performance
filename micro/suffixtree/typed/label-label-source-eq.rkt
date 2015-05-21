#lang typed/racket/base

(provide label-source-eq?)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-same-source.rkt"
  [label-same-source? (-> label label Boolean)])

;; =============================================================================

;; --- from suffixtree.rkt
(define label-source-eq? label-same-source?)
