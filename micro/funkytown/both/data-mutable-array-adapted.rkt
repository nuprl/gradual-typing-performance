#lang typed/racket/base

(provide (struct-out Mutable-Array))

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-settable-array-adapted.rkt")

;; =============================================================================

(require/typed/check "data-mutable-array.rkt"
  [#:struct (Mutable-Array Settable-Array)
            ([data : (Vectorof Float)])])
