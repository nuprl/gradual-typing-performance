#lang racket/base

(provide label-source-eq?)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-same-source.rkt" label-same-source?))

;; =============================================================================

;; --- from suffixtree.rkt
(define label-source-eq? label-same-source?)
