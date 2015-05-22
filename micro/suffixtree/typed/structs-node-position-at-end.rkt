#lang typed/racket/base

(provide node-position-at-end?)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-ref-at-end.rkt"
  [label-ref-at-end? (-> label Integer Boolean)])

;; =============================================================================

;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(: node-position-at-end? (-> Node Index Boolean))
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))
