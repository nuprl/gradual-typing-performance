#lang typed/racket/base

(provide longest-common-substring)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-label-string.rkt"
  [label->string (-> Label String)])
(require/typed/check "label-string-label-with-sentinel.rkt"
  [string->label/with-sentinel (-> String label)])
(require/typed/check "lcs-longest-common-sublabel.rkt"
  [longest-common-sublabel (-> Label Label Label)])

;; =============================================================================

;; longest-common-substring: string string -> string
;; Returns the longest common substring between the two strings.
(: longest-common-substring (-> String String String))
(define (longest-common-substring s1 s2)
  (label->string (longest-common-sublabel (string->label/with-sentinel s1)
                                          (string->label/with-sentinel s2))))
