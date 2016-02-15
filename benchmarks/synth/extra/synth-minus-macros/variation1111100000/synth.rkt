#lang racket/base

(provide
  fs
  sawtooth-wave
  seconds->samples
  emit)

(require
  (only-in racket/math exact-floor)
  (only-in "array-struct.rkt" array-strictness array-size)
  "array-sequence.rkt")

;; -- synth

;; TODO this slows down a bit, it seems, but improves memory use
(array-strictness #f)

(define fs 44100)
(define bits-per-sample 16)

(define (freq->sample-period freq)
  (round (/ fs freq)))

(define (seconds->samples s)
  (inexact->exact (round (* s fs))))

;; --- Oscillators

;; array functions receive a vector of indices
(define-syntax-rule (array-lambda (i) body ...)
  (lambda (i*) (let ([i (vector-ref i* 0)]) body ...)))

(define ((make-sawtooth-wave coeff) freq)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; gradually goes from -1 to 1 over the whole cycle
    (define x* (exact->inexact (modulo x sample-period)))
    (* coeff (- (/ x* sample-period/2) 1.0))))
(define sawtooth-wave         (make-sawtooth-wave 1.0))

;; --- Emit

;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
(define (signal->integer-sequence signal #:gain [gain 1])
  (for-array/vector signal
             ; #:length (array-size signal)
             ; ([sample (in-array signal)])
  (lambda (sample)
    (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample))))))))))

;; `emit` used to write a file.
;; For now, it just converts a signal to a sequence.
(define (emit signal)
  (signal->integer-sequence signal #:gain 0.3))

