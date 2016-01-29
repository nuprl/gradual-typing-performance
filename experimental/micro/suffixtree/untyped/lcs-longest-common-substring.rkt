#lang racket/base

(provide longest-common-substring)

;; -----------------------------------------------------------------------------

(require "data-label.rkt"
(only-in "label-label-string.rkt" label->string)
(only-in "label-string-label-with-sentinel.rkt" string->label/with-sentinel)
(only-in "lcs-longest-common-sublabel.rkt" longest-common-sublabel))

;; =============================================================================

;; longest-common-substring: string string -> string
;; Returns the longest common substring between the two strings.
(define (longest-common-substring s1 s2)
  (label->string (longest-common-sublabel (string->label/with-sentinel s1)
                                          (string->label/with-sentinel s2))))
