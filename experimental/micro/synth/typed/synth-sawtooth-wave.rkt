#lang typed/racket/base

(provide sawtooth-wave)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "type-aliases.rkt")
(require/typed/check "synth-make-sawtooth-wave.rkt"
  [make-sawtooth-wave (-> Float (-> Float (-> Indexes Float)))])

;; =============================================================================

(: sawtooth-wave (-> Float (-> Indexes Float)))
(define sawtooth-wave         (make-sawtooth-wave 1.0))
