#lang typed/racket/base

(provide vector-copy-all)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt"
         benchmark-util)
(require/typed/check "array-utils-vector-supertype-vector.rkt"
  [vector->supertype-vector (-> Indexes Indexes)])

;; =============================================================================

(begin-encourage-inline
  (: vector-copy-all (-> Indexes Indexes))
  (define (vector-copy-all js) (vector->supertype-vector js))
)
