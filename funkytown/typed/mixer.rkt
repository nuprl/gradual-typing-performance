#lang typed/racket/base

(require (only-in "array-pointwise.rkt" array-map)
         (only-in "array-struct.rkt" Array)
         (only-in "array-broadcast.rkt" array-broadcasting)
         (only-in racket/list first second rest))

(provide mix Weighted-Signal)

;; A Weighted-Signal is a (List (Array Float) Real)
(define-type Weighted-Signal (List (Array Float) Real))

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

(: mix (-> Weighted-Signal * (Array Float)))
(define (mix . ss)
  (: signals (Listof (Array Float)))
  (define signals
    (for/list : (Listof (Array Float)) ([s : Weighted-Signal ss])
      (first s)))
  (: weights (Listof Float))
  (define weights
    (for/list : (Listof Float) ([x : Weighted-Signal ss])
      (real->double-flonum (second x))))
  (: downscale-ratio Float)
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (: scale-signal (Float -> (Float -> Float)))
  (define ((scale-signal w) x) (* x w downscale-ratio))
  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res (array-map (scale-signal (first weights))
                               (first signals))])
        ([s (in-list (rest signals))]
         [w (in-list (rest weights))])
      (define scale (scale-signal w))
      (array-map (lambda ([acc : Float]
                          [new : Float])
                   (+ acc (scale new)))
                 res s))))
