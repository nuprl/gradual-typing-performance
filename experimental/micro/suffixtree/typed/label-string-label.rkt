#lang typed/racket/base

(provide string->label)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-vector-label.rkt"
  [vector->label (-> (Vectorof (U Char Symbol)) label)])

;; =============================================================================

;;string->label: string -> label
;;Constructs a new label from the input string.
(: string->label (-> String label))
(define (string->label str)
  (vector->label (list->vector (string->list str))))
