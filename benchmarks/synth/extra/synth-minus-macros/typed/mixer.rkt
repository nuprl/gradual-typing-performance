#lang typed/racket/base

(require benchmark-util
  "array-pointwise.rkt"
         "typed-data.rkt"
         (only-in racket/list first second rest))

(require/typed/check "array-broadcast.rkt"
  [array-broadcasting (Parameterof (U #f #t 'permissive))])

(provide mix)

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

(: mix (-> Weighted-Signal * Array))
(define (mix . ss)
  (: signals (Listof Array))
  (define signals
    (for/list : (Listof Array) ([s : Weighted-Signal ss])
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
    (for/fold ([res : Array (array-map (scale-signal (first weights))
                               (first signals))])
        ([s (in-list (rest signals))]
         [w (in-list (rest weights))])
      (define scale (scale-signal w))
      (array-map (lambda ([acc : Float]
                          [new : Float])
                   (+ acc (scale new)))
                 res s))))
