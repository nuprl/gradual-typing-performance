#lang racket/base

(provide make-sawtooth-wave)

;; -----------------------------------------------------------------------------

(require (only-in "synth-freq-sample-period.rkt" freq->sample-period))

;; =============================================================================

(define ((make-sawtooth-wave coeff) freq)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; gradually goes from -1 to 1 over the whole cycle
    (define x* (exact->inexact (modulo x sample-period)))
    (* coeff (- (/ x* sample-period/2) 1.0))))

;; -----------------------------------------------------------------------------

;; array functions receive a vector of indices
(define-syntax-rule (array-lambda (i) body ...)
  (lambda (i*)
    (let ([i (vector-ref i* 0)]) body ...)))
