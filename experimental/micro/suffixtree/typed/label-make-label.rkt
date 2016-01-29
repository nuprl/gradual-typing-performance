#lang typed/racket/base

(provide make-label)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-string-label.rkt"
  [string->label (-> String label)])
(require/typed/check "label-vector-label.rkt"
  [vector->label (-> (Vectorof (U Char Symbol)) label)])

;; =============================================================================

;;make-label: label-element -> label
;;Constructs a new label from either a string or a vector of things.
(: make-label (-> (U String (Vectorof (U Char Symbol))) label))
(define (make-label label-element)
 (cond ((string? label-element) (string->label label-element))
       ((vector? label-element) (vector->label label-element))
       (else
        (error 'make-label "Don't know how to make label from ~S" label-element))))
