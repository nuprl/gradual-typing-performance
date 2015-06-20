#lang racket/base

(provide
  log2
  natural->binary
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/math exact-ceiling)
  (only-in racket/format ~r)
)

;; =============================================================================

;; log, base 2
;; (: log2 (-> Integer Flonum))
(define (log2 n)
  (exact-ceiling (/ (log n) (log 2))))

;; Convert a natural number to a binary string, padded to the supplied width
;; (: natural->binary (-> Index Index String))
(define (natural->binary n pad-width)
  (~r n #:base 2 #:min-width pad-width #:pad-string "0"))

