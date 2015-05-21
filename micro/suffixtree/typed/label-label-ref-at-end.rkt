#lang typed/racket/base

(provide label-ref-at-end?)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])

;; =============================================================================

;; label-ref-at-end?: label number -> boolean
(: label-ref-at-end? (-> label Integer Boolean))
(define (label-ref-at-end? label offset)
  (= offset (label-length label)))
