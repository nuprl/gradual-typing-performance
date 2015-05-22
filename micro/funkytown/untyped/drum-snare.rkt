#lang racket/base

(provide snare)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
(only-in "drum-random-sample.rkt" random-sample)
(only-in "synth-seconds-samples.rkt" seconds->samples)
(only-in "array-struct-build-array.rkt" build-array))

;; =============================================================================

(define snare
  ;; 0.05 seconds of noise
  (let ([indexes
                  (vector (seconds->samples 0.05))]
         [arr-gen
                  (lambda (x) (random-sample))])
    (build-array indexes arr-gen)))
