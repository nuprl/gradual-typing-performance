#lang racket/base

(provide label->string)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-vector.rkt" label->vector))

;; =============================================================================

;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.
(define (label->string label)
  (define V (label->vector label))
  (define L (for/list
                      ([c (in-vector V)])
              (unless (char? c) (error "label->string invariant broken"))
              c))
  (list->string L))
