#lang typed/racket/base

(provide skip-count)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-length.rkt"
  [label-length (-> label Index)])
(require/typed/check "ukkonen-skip-count-helper.rkt"
  [skip-count-helper (-> Node Label Index Index (values Node Index))])

;; =============================================================================

;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(: skip-count (-> Node Label (values Node Index)))
(define (skip-count node label)
  (define l-l (label-length label))
  (unless (index? l-l) (error "skip-count"))
  (skip-count-helper node label 0 l-l))
