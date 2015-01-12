#lang racket

(require (only-in "math/array.rkt"
                  array?
                  array-size
                  array-strictness
                  in-array)
         racket/contract)

(require "wav-encode.rkt") ;; TODO does not accept arrays directly

;; TODO try to get deforestation for arrays. does that require
;;   non-strict arrays? lazy arrays?
(array-strictness #f)
;; TODO this slows down a bit, it seems, but improves memory use


(provide fs seconds->samples)

(define fs 44100)
(define bits-per-sample 16)

(define/contract
  (freq->sample-period freq)
  (-> flonum? integer?)
  (round (/ fs freq)))

(define/contract
  (seconds->samples s)
  (-> flonum? natural-number/c)
  (inexact->exact (round (* s fs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Oscillators

(provide sine-wave square-wave sawtooth-wave inverse-sawtooth-wave
         triangle-wave)

;; array functions receive a vector of indices
(define-syntax-rule (array-lambda (i) body ...)
  (lambda (i*) (let ([i (vector-ref i* 0)]) body ...)))

;; These all need to return floats.
;; TODO use TR? would also optimize for us

(define/contract
  (sine-wave freq)
  (-> natural-number/c natural-number/c)
  (define f (exact->inexact (/ (* freq 2.0 pi) fs)))
  (array-lambda (x) (sin (* f (exact->inexact x)))))

(define/contract
  (square-wave freq)
  (-> natural-number/c natural-number/c)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; 1 for the first half of the cycle, -1 for the other half
    (define x* (modulo x sample-period))
    (if (> x* sample-period/2) -1.0 1.0)))


(define/contract
  ((make-sawtooth-wave coeff) freq)
  (-> (real-in -1 1) (-> flonum? (-> vector? (real-in -1 1))))
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; gradually goes from -1 to 1 over the whole cycle
    (define x* (exact->inexact (modulo x sample-period)))
    (* coeff (- (/ x* sample-period/2) 1.0))))
(define sawtooth-wave         (make-sawtooth-wave 1.0))
(define inverse-sawtooth-wave (make-sawtooth-wave -1.0))

(define/contract
  (triangle-wave freq)
  (-> flonum? natural-number/c)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (define sample-period/4 (quotient sample-period 4))
  (array-lambda (x)
    ;; go from 1 to -1 for the first half of the cycle, then back up
    (define x* (modulo x sample-period))
    (if (> x* sample-period/2)
        (- (/ x* sample-period/4) 3.0)
        (+ (/ x* sample-period/4 -1.0) 1.0))))

;; TODO make sure that all of these actually produce the right frequency
;;  (i.e. no off-by-an-octave errors)

;; TODO add weighted-harmonics, so we can approximate instruments
;;  and take example from old synth

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide emit plot-signal)

;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
(define/contract
  (signal->integer-sequence signal #:gain [gain 1])
  (->* (array?) (#:gain (real-in -1 1)) (vectorof natural-number/c))
  (for/vector #:length (array-size signal)
              ([sample (in-array signal)])
    (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample)))))))))


(require plot)
(plot-new-window? #t)
;; shows 2 plots
;; - the original signal
;; - the "digitized" signal, with series for the bounds
;; press any key to dismiss
(define/contract
  (plot-signal signal)
  (-> none/c none/c)
  (define n (array-size signal))
  (plot (points (for/list ([s (in-array signal)]
                           [i (in-naturals)])
                  (vector i s))))
  (plot (list (points (for/list ([s (in-vector
                                     (signal->integer-sequence signal))]
                                 [i (in-naturals)])
                        (vector i s)))
              (points (for/list ([i (in-range n)])
                        (vector i 0)))
              (points (for/list ([i (in-range n)])
                        (vector i (expt 2 bits-per-sample))))))
  (read-char))


(define/contract
  (emit signal file)
  (-> array? string? void?)
  (with-output-to-file file #:exists 'replace
    (lambda () (write-wav (signal->integer-sequence signal #:gain 0.3)))))
