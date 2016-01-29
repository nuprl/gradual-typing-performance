#lang racket/base

(provide make-label)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-string-label.rkt" string->label)
(only-in "label-vector-label.rkt" vector->label))

;; =============================================================================

;;make-label: label-element -> label
;;Constructs a new label from either a string or a vector of things.
(define (make-label label-element)
 (cond ((string? label-element) (string->label label-element))
       ((vector? label-element) (vector->label label-element))
       (else
        (error 'make-label "Don't know how to make label from ~S" label-element))))
