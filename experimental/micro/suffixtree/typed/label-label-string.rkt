#lang typed/racket/base

(provide label->string)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-vector.rkt"
  [label->vector (-> label (Vectorof (U Char Symbol)))])

;; =============================================================================

;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->string (-> label String))
(define (label->string label)
  (: V (Vectorof (U Char Symbol)))
  (define V (label->vector label))
  (: L (Listof Char))
  (define L (for/list : (Listof Char)
                      ([c : (U Char Symbol) (in-vector V)])
              (unless (char? c) (error "label->string invariant broken"))
              c))
  (list->string L))
