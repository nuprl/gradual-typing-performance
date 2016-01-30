#lang racket/base

(provide label-copy)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; label-copy: label->label
;; Returns a copy of the label.
(define (label-copy lbl)
  (label (label-datum lbl) (label-i lbl) (label-j lbl)))
