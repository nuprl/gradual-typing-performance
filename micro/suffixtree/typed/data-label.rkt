#lang typed/racket/base

(provide (struct-out label))

;; -----------------------------------------------------------------------------
;; =============================================================================

(struct label
  ([datum : (Vectorof (U Char Symbol))]
   [i : Natural]
   [j : Natural])
  #:mutable)
