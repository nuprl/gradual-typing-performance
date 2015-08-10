#lang typed/racket/base

(provide make-sawtooth-wave)

;; -----------------------------------------------------------------------------

(require benchmark-util
         "type-aliases.rkt")
(require/typed/check "synth-freq-sample-period.rkt"
  [freq->sample-period (-> Float Integer)])

;; =============================================================================

(: make-sawtooth-wave (-> Float (-> Float (-> Indexes Float))))
(define ((make-sawtooth-wave coeff) freq)
  (: sample-period Integer)
  (define sample-period (freq->sample-period freq))
  (: sample-period/2 Integer)
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; gradually goes from -1 to 1 over the whole cycle
    (: x* Float)
    (define x* (exact->inexact (modulo x sample-period)))
    (* coeff (- (/ x* sample-period/2) 1.0))))

;; -----------------------------------------------------------------------------

;; array functions receive a vector of indices
(define-syntax-rule (array-lambda (i) body ...)
  (lambda ([i* : (Vectorof Integer)])
    (let: ([i : Integer (vector-ref i* 0)]) body ...)))
