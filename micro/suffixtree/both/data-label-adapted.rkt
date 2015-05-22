#lang typed/racket/base

(provide (struct-out label)
         set-label-j!
         set-label-i!
         Label)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "data-label.rkt"
  [#:struct label ([datum : (Vectorof (U Char Symbol))]
                   [i : Natural]
                   [j : Natural])]
  [set-label-j! (-> label Natural Void)]
  [set-label-i! (-> label Natural Void)])

;; =============================================================================

(define-type Label label)
