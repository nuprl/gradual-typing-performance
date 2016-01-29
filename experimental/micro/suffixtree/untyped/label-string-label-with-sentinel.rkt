#lang racket/base

(provide string->label/with-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-vector-label-with-sentinel.rkt" vector->label/with-sentinel))

;; =============================================================================

;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(define (string->label/with-sentinel str)
  (vector->label/with-sentinel (list->vector (string->list str))))
