#lang racket/base

(provide unsafe-build-simple-array)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-utils-check-array-shape-size.rkt"
  check-array-shape-size))

;; =============================================================================

(define (unsafe-build-simple-array ds f)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))
