#lang racket/base

(provide string->label)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-vector-label.rkt" vector->label))

;; =============================================================================

;;string->label: string -> label
;;Constructs a new label from the input string.
(define (string->label str)
  (vector->label (list->vector (string->list str))))
