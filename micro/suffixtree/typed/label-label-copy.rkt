#lang typed/racket/base

(provide label-copy)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; label-copy: label->label
;; Returns a copy of the label.
(: label-copy (-> label label))
(define (label-copy lbl)
  (label (label-datum lbl) (label-i lbl) (label-j lbl)))
