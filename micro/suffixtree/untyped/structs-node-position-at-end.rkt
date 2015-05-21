#lang racket/base

(provide node-position-at-end?)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-ref-at-end.rkt" label-ref-at-end?))

;; =============================================================================

;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))
