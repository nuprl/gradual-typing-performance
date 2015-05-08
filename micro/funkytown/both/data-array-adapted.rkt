#lang typed/racket/base

(provide (struct-out Array))

;; -----------------------------------------------------------------------------

(require benchmark-util)

;; =============================================================================

(require/typed/check "data-array.rkt"
  [#:struct Array ([shape : (Vectorof Integer)]
                   [size : Integer]
                   [strict? : (Boxof Boolean)]
                   [strict! : (-> Void)]
                   [unsafe-proc : (-> (Vectorof Integer) Float)])])
