#lang typed/racket/base

(provide snare)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         "type-aliases.rkt"
         benchmark-util)
(require/typed/check "drum-random-sample.rkt"
  [random-sample (-> Float)])
(require/typed/check "synth-seconds-samples.rkt"
  [seconds->samples (-> Float Integer)])
(require/typed/check "array-struct-build-array.rkt"
  [build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)])

;; =============================================================================

(: snare Array)
(define snare
  ;; 0.05 seconds of noise
  (let: ([indexes : In-Indexes
                  (vector (seconds->samples 0.05))]
         [arr-gen : (-> Indexes Flonum)
                  (lambda ([x : Indexes]) (random-sample))])
    (build-array indexes arr-gen)))
