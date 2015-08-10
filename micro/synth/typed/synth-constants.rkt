#lang typed/racket/base

(provide fs
         bits-per-sample)

;; =============================================================================

(: fs Natural)
(define fs 44100)
(: bits-per-sample Natural)
(define bits-per-sample 16)
