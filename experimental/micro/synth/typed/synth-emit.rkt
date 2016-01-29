#lang typed/racket/base

(provide emit)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "synth-signal-integer-sequence.rkt"
  [signal->integer-sequence (-> Array [#:gain Float] (Vectorof Integer))])

;; =============================================================================

;; `emit` used to write a file.
;; For now, it just converts a signal to a sequence.
(: emit (-> Array (Vectorof Integer)))
(define (emit signal)
  (signal->integer-sequence signal #:gain 0.3))
