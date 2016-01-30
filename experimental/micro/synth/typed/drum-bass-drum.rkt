#lang typed/racket/base

(provide bass-drum)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         "type-aliases.rkt"
         benchmark-util)
(require/typed/check "synth-signal-integer-sequence.rkt"
  [signal->integer-sequence (-> Array [#:gain Float] (Vectorof Integer))])
(require/typed/check "array-struct-unsafe-vector-array.rkt"
  [unsafe-vector->array (Indexes (Vectorof Float) -> Mutable-Array)])
(require/typed/check "drum-random-sample.rkt"
  [random-sample (-> Float)])
(require/typed/check "synth-seconds-samples.rkt"
  [seconds->samples (-> Float Integer)])
(require/typed/check "array-utils-check-array-shape.rkt"
  [check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes)])
(require/typed/check "array-utils-array-shape-size.rkt"
  [array-shape-size (Indexes -> Integer)])

;; =============================================================================

;; Drum "samples" (Arrays of floats)
(: bass-drum Array)
(define bass-drum
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (: n-samples Integer)
    (define n-samples           (seconds->samples 0.05))
    (: n-different-samples Integer)
    (define n-different-samples (quotient n-samples 12))
    (: ds* In-Indexes)
    (define ds* (vector n-samples))
    (: ds  Indexes)
    (define ds
      (check-array-shape ds*
                         (λ () (raise-argument-error 'name "Indexes" ds))))
    (: vs (Vectorof Flonum))
    (define vs
      (for/vector : (Vectorof Flonum)
                  #:length (array-shape-size ds)
                  #:fill 0.0
                  ([i      : Natural (in-range n-different-samples)]
                   [sample : Flonum  (in-producer random-sample)]
                   #:when #t
                   [j      : Natural (in-range 12)])
                  sample))
    (unsafe-vector->array ds vs)))
