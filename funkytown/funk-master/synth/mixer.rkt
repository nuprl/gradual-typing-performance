#lang racket

(require (only-in "math/array.rkt"
                  array-broadcasting
                  array-map))

(provide mix)

;; A Weighted-Signal is a (List (Array Float) Real)

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

;; mix : Weighted-Signal * -> (Array Float)
(define (mix . ss)

  (define signals (map (lambda (x) ; : Weighted-Signal
                         (first x))
                       ss))
  (define weights (map (lambda (x) ; : Weighted-Signal
                         (real->double-flonum (second x)))
                       ss))
  (define downscale-ratio (/ 1.0 (apply + weights)))

  ;; scale-signal : Float -> (Float -> Float)
  (define ((scale-signal w) x) (* x w downscale-ratio))

  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res (array-map (scale-signal (first weights))
                               (first signals))])
        ([s (in-list (rest signals))]
         [w (in-list (rest weights))])
      (define scale (scale-signal w))
      (array-map (lambda (acc  ; : Float
                          new) ; : Float
                   (+ acc (scale new)))
                 res s))))
