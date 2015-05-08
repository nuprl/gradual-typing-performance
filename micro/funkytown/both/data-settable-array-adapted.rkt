#lang typed/racket/base

(provide (struct-out Settable-Array))

;; -----------------------------------------------------------------------------

(require benchmark-util
         "data-array-adapted.rkt")

;; =============================================================================

(require/typed/check "data-settable-array.rkt"
  [#:struct (Settable-Array Array)
            ([set-proc : ((Vectorof Integer) Float -> Void)])])
