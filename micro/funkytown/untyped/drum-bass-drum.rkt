#lang racket/base

(provide bass-drum)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
(only-in "synth-signal-integer-sequence.rkt" signal->integer-sequence)
(only-in "array-struct-unsafe-vector-array.rkt" unsafe-vector->array)
(only-in "drum-random-sample.rkt" random-sample)
(only-in "synth-seconds-samples.rkt" seconds->samples)
(only-in "array-utils-check-array-shape.rkt" check-array-shape)
(only-in "array-utils-array-shape-size.rkt" array-shape-size))

;; =============================================================================

;; Drum "samples" (Arrays of floats)
(define bass-drum
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (define n-samples           (seconds->samples 0.05))
    (define n-different-samples (quotient n-samples 12))
    (define ds* (vector n-samples))
    (define ds
      (check-array-shape ds*
                         (λ () (raise-argument-error 'name "Indexes" ds))))
    (define vs
      (for/vector
                  #:length (array-shape-size ds)
                  #:fill 0.0
                  ([i      (in-range n-different-samples)]
                   [sample  (in-producer random-sample)]
                   #:when #t
                   [j      (in-range 12)])
                  sample))
    (unsafe-vector->array ds vs)))
