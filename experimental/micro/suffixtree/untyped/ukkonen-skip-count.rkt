#lang racket/base

(provide skip-count)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-label-length.rkt"
  label-length)
(only-in "ukkonen-skip-count-helper.rkt"
  skip-count-helper))

;; =============================================================================

;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(define (skip-count node label)
  (define l-l (label-length label))
  (unless (integer? l-l) (error "skip-count"))
  (skip-count-helper node label 0 l-l))
