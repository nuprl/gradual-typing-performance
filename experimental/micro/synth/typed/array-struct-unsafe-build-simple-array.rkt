#lang typed/racket/base

(provide unsafe-build-simple-array)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "array-utils-check-array-shape-size.rkt"
  [check-array-shape-size (-> Symbol Indexes Integer)])

;; =============================================================================

(: unsafe-build-simple-array (Indexes (Indexes -> Float) -> Array))
(define (unsafe-build-simple-array ds f)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))
