#lang typed/racket/base

(provide string->label/with-sentinel)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-vector-label-with-sentinel.rkt"
  [vector->label/with-sentinel (-> (Vectorof (U Char Symbol)) label)])

;; =============================================================================

;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(: string->label/with-sentinel (-> String label))
(define (string->label/with-sentinel str)
  (vector->label/with-sentinel (list->vector (string->list str))))
